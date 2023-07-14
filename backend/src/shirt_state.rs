use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, Hash, Deserialize, Serialize, Clone, Default)]
pub struct ShirtSize(String);

impl ShirtSize {
    pub fn new(string: String) -> Self {
        Self(string)
    }
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Deserialize, Serialize, Clone)]
pub struct Name(String);

impl Name {
    pub fn new(mut string: String) -> Name {
        string.truncate(100);
        Name(string)
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
#[derive(Debug, PartialEq, Eq, Hash, Deserialize, Serialize, Clone)]
pub struct UserData {
    is_admin: bool,
    name: Name,
    vote: Option<ShirtSize>,
}

#[derive(Debug, PartialEq, Eq, Hash, Deserialize, Serialize, Clone)]
pub struct UserId(String);

impl UserId {
    pub fn from_str(input: &str) -> Self {
        Self(input.into())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Deserialize, Serialize, Clone)]
pub enum Votes {
    VotesHidden {
        own_vote: Option<ShirtSize>,
        votes: Vec<(Name, bool)>,
    },
    VotesRevealed {
        own_vote: Option<ShirtSize>,
        votes: Vec<(Name, Option<ShirtSize>)>,
    },
}

impl Votes {
    pub fn find_name(&self, needle: &Name) -> bool {
        match self {
            Votes::VotesHidden { votes, .. } => votes.iter().any(|(name, _)| name == needle),
            Votes::VotesRevealed { votes, .. } => votes.iter().any(|(name, _)| name == needle),
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub mod shirt_service {
    use axum::response::sse::{Event, KeepAlive, Sse};
    use axum::{routing::get, Router};
    use axum::{routing::put, Json};
    use axum_sessions::extractors::ReadableSession;
    use axum_sessions::{async_session::MemoryStore, extractors::WritableSession, SessionLayer};
    use futures::StreamExt;
    use futures::{stream, Stream};
    use parking_lot::RwLock;
    use std::{collections::HashMap, sync::Arc};
    use tokio::sync::broadcast;
    use tracing::info;
    use uuid::Uuid;

    use crate::{
        auth::{BearerToken, BearerValidation},
        Error,
    };

    use super::{Name, ShirtSize, UserData, UserId, Votes};

    #[derive(Debug, Clone, Default)]
    struct ShirtSizeState {
        sizes: Arc<RwLock<Vec<ShirtSize>>>,
        revealed: Arc<RwLock<bool>>,
        users: Arc<RwLock<HashMap<UserId, UserData>>>,
    }

    pub struct ShirtSizeService {
        state: ShirtSizeState,
        sender: broadcast::Sender<()>,
    }

    impl ShirtSizeService {
        pub fn new() -> ShirtSizeService {
            let (sender, _) = broadcast::channel(10000);
            ShirtSizeService {
                sender,
                state: ShirtSizeState::default(),
            }
        }

        /// /vote PUT
        /// /name PUT
        /// /votes DELETE
        /// /register PUT
        /// /shirt-sizes PUT
        pub fn routes(
            &self,
            session_layer: SessionLayer<MemoryStore>,
            bearer_validation: &'static BearerValidation,
        ) -> Router {
            let state = self.state.clone();
            let sender = self.sender.clone();
            let votes_handler =
                move |session: ReadableSession| sse_handler(state, session, sender.subscribe());
            let state = self.state.clone();
            let state_for_reset = self.state.clone();
            let state_for_reveal = self.state.clone();
            let state_for_vote_handler = self.state.clone();
            let state_for_sizes = self.state.clone();
            let state_for_put_sizes = self.state.clone();
            let mut tx = self.sender.subscribe();
            tokio::spawn(async move {
                while let Ok(()) = tx.recv().await {
                    info!("Received update");
                }
            });
            let sender = self.sender.clone();
            let sender_for_vote_handler = self.sender.clone();
            let sender_for_sizes = self.sender.clone();
            let sender_for_reset = self.sender.clone();
            let sender_for_reveal = self.sender.clone();

            Router::new()
                .route("/votes-sse", get(votes_handler))
                .route(
                    "/reveal",
                    get(|token: BearerToken| {
                        reveal_handler(
                            state_for_reveal,
                            sender_for_reveal,
                            bearer_validation,
                            token,
                        )
                    }),
                )
                .route(
                    "/reset",
                    get(|token: BearerToken| {
                        reset_handler(state_for_reset, sender_for_reset, bearer_validation, token)
                    }),
                )
                .route("/sizes", get(|| sizes_handler(state_for_sizes)))
                .route(
                    "/sizes",
                    put(|Json(data), token: BearerToken| {
                        sizes_put_handler(
                            data,
                            state_for_put_sizes,
                            sender_for_sizes,
                            bearer_validation,
                            token,
                        )
                    }),
                )
                .route(
                    "/login",
                    put(|Json(data), session: WritableSession, token: BearerToken| {
                        login_handler(data, sender, state, session, bearer_validation, token)
                    }),
                )
                .route(
                    "/vote",
                    put(|Json(data), session: ReadableSession| {
                        vote_handler(
                            state_for_vote_handler,
                            data,
                            sender_for_vote_handler,
                            session,
                        )
                    }),
                )
                .layer(session_layer)
        }
    }

    async fn vote_handler(
        state: ShirtSizeState,
        vote: ShirtSize,
        sender: broadcast::Sender<()>,
        session: ReadableSession,
    ) -> Result<(), Error> {
        let current_id = session
            .get::<UserId>("id")
            .ok_or_else(|| Error::Message("Failed to find user".into()))?;
        let mut users = state.users.write();
        info!("USERS FOR VOTE: {:?}", *users);
        {
            let data = {
                users
                    .get_mut(&current_id)
                    .ok_or_else(|| Error::Message("Failed to find user".into()))?
            };

            data.vote = Some(vote);
        }
        sender.send(())?;
        Ok(())
    }

    async fn reset_handler(
        state: ShirtSizeState,
        sender: broadcast::Sender<()>,
        bearer_validation: &BearerValidation,
        token: BearerToken,
    ) -> Result<Json<()>, Error> {
        bearer_validation.authorise(token)?;
        {
            let mut revealed = state.revealed.write();
            *revealed = false;
            let mut users = state.users.write();
            for (_, data) in users.iter_mut() {
                data.vote = None;
            }
            sender.send(())?;
        }
        Ok(Json(()))
    }
    async fn reveal_handler(
        state: ShirtSizeState,
        sender: broadcast::Sender<()>,
        bearer_validation: &BearerValidation,
        token: BearerToken,
    ) -> Result<Json<()>, Error> {
        bearer_validation.authorise(token)?;
        let mut revealed = state.revealed.write();
        *revealed = true;
        sender.send(())?;
        Ok(Json(()))
    }

    async fn sizes_handler(state: ShirtSizeState) -> Result<Json<Vec<ShirtSize>>, Error> {
        Ok(Json(state.sizes.read().to_vec()))
    }

    // todo: add auth
    async fn sizes_put_handler(
        new_sizes: Vec<ShirtSize>,
        state: ShirtSizeState,
        sender: broadcast::Sender<()>,
        bearer_validation: &BearerValidation,
        token: BearerToken,
    ) -> Result<(), Error> {
        bearer_validation.authorise(token)?;
        {
            let mut sizes = state.sizes.write();
            *sizes = new_sizes;
        }
        {
            state.users.write().clear();
        }
        sender.send(())?;
        Ok(())
    }

    async fn login_handler(
        name: Name,
        sender: broadcast::Sender<()>,
        state: ShirtSizeState,
        mut session: WritableSession,
        bearer_validation: &BearerValidation,
        token: BearerToken,
    ) -> Result<Json<bool>, Error> {
        let is_admin = bearer_validation.authorise(token).is_ok();
        info!("USER IS ADMIN: {is_admin}");
        {
            let user_id = match session.get::<UserId>("id") {
                Some(got_user_id) => got_user_id,
                None => {
                    let new_user_id = Uuid::new_v4().to_string();
                    session.insert("id", new_user_id.clone())?;
                    UserId(new_user_id)
                }
            };
            session.insert("name", name.clone())?;
            let mut users = state.users.write();
            match users.get_mut(&user_id) {
                Some(user) => user.name = name,
                None => {
                    users.insert(
                        user_id,
                        UserData {
                            is_admin,
                            name,
                            vote: None,
                        },
                    );
                }
            };
        };
        sender.send(())?;
        Ok(Json(is_admin))
    }

    fn get_votes(state: &ShirtSizeState, own_id: &UserId) -> Result<Votes, Error> {
        let visible = { *state.revealed.read() };
        let votes = state.users.read();
        let own_vote = votes.get(own_id).and_then(|data| data.vote.clone());
        let votes_iter = votes.iter();

        let votes = if visible {
            let votes: Vec<(Name, Option<ShirtSize>)> = votes_iter
                .map(|(_, user_data)| {
                    let UserData { name, vote, .. } = user_data;
                    (name.clone(), vote.clone())
                })
                .collect::<Vec<_>>();
            Votes::VotesRevealed { own_vote, votes }
        } else {
            let votes: Vec<(Name, bool)> = votes_iter
                .map(|(_, user_data)| {
                    let UserData { name, vote, .. } = user_data;
                    (name.clone(), vote.is_some())
                })
                .collect::<Vec<_>>();
            Votes::VotesHidden { own_vote, votes }
        };
        Ok(votes)
    }

    async fn sse_handler(
        state: ShirtSizeState,
        session: ReadableSession,
        receiver: broadcast::Receiver<()>,
    ) -> Result<Sse<impl Stream<Item = Result<Event, Error>>>, Error> {
        let user_id = session
            .get::<UserId>("id")
            .ok_or_else(|| Error::Message("Session not set".into()))?;
        info!("Opening SSE connection for {:?}", user_id);
        let initial = get_votes(&state, &user_id);
        let stream = stream::once(async { initial })
            .chain(
                tokio_stream::wrappers::BroadcastStream::new(receiver)
                    .map(move |_| get_votes(&state, &user_id)),
            )
            .map(|votes| {
                let votes = votes?;
                Ok(Event::default().json_data(&votes)?)
            });
        Ok(Sse::new(stream).keep_alive(KeepAlive::default()))
    }
}

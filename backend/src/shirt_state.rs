use crate::Error;
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
    use axum::{
        extract::ws::{Message, WebSocket, WebSocketUpgrade},
        response::{IntoResponse, Response},
        routing::get,
        Router,
    };
    use axum::{routing::put, Json};
    use axum_sessions::extractors::ReadableSession;
    use axum_sessions::{async_session::MemoryStore, extractors::WritableSession, SessionLayer};
    use futures::stream::SplitSink;
    use futures::{stream::SplitStream, SinkExt, StreamExt};
    use std::{
        collections::HashMap,
        sync::{Arc, RwLock},
    };
    use tokio::sync::broadcast;
    use tracing::{info, warn};
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
        secret: Arc<BearerValidation>,
    }

    impl ShirtSizeService {
        pub fn new(secret: &str) -> ShirtSizeService {
            let (sender, _) = broadcast::channel(10000);
            ShirtSizeService {
                sender,
                state: ShirtSizeState::default(),
                secret: Arc::new(BearerValidation::new(secret)),
            }
        }

        /// /vote PUT
        /// /name PUT
        /// /votes DELETE
        /// /register PUT
        /// /shirt-sizes PUT
        pub fn routes(&self, session_layer: SessionLayer<MemoryStore>) -> Router {
            let state = self.state.clone();
            let sender = self.sender.clone();
            let votes_handler =
                move |ws, session: ReadableSession| votes_handler(state, sender, session, ws);
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

            let secret_for_sizes = self.secret.clone();
            let secret_for_login = self.secret.clone();
            let secret_for_reset = self.secret.clone();
            let secret_for_reveal = self.secret.clone();
            Router::new()
                .route("/votes-ws", get(votes_handler))
                .route(
                    "/reveal",
                    get(|token: BearerToken| {
                        reveal_handler(
                            state_for_reveal,
                            sender_for_reveal,
                            secret_for_reveal,
                            token,
                        )
                    }),
                )
                .route(
                    "/reset",
                    get(|token: BearerToken| {
                        reset_handler(state_for_reset, sender_for_reset, secret_for_reset, token)
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
                            secret_for_sizes,
                            token,
                        )
                    }),
                )
                .route(
                    "/login",
                    put(|Json(data), session: WritableSession, token: BearerToken| {
                        login_handler(data, sender, state, session, secret_for_login, token)
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
        let mut users = state
            .users
            .write()
            .expect("Users rw lock is poisoned. Cannot proceed");
        info!("USERS FOR VOTE: {:?}", *users);
        let data = {
            users
                .get_mut(&current_id)
                .ok_or_else(|| Error::Message("Failed to find user".into()))?
        };

        data.vote = Some(vote);
        sender.send(())?;
        Ok(())
    }
    async fn reset_handler(
        state: ShirtSizeState,
        sender: broadcast::Sender<()>,
        bearer_validation: Arc<BearerValidation>,
        token: BearerToken,
    ) -> Result<Json<()>, Error> {
        bearer_validation.authorise(token)?;
        {
            let mut revealed = state
                .revealed
                .write()
                .expect("Shirt sizes lock poisoned, can't recover");
            *revealed = false;
            let mut users = state
                .users
                .write()
                .expect("Shirt sizes lock poisoned, can't recover");
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
        bearer_validation: Arc<BearerValidation>,
        token: BearerToken,
    ) -> Result<Json<()>, Error> {
        bearer_validation.authorise(token)?;
        let mut revealed = state
            .revealed
            .write()
            .expect("Shirt sizes lock poisoned, can't recover");
        *revealed = true;
        sender.send(())?;
        Ok(Json(()))
    }

    async fn sizes_handler(state: ShirtSizeState) -> Result<Json<Vec<ShirtSize>>, Error> {
        Ok(Json(
            state
                .sizes
                .read()
                .expect("Shirt sizes lock is poisoned, can't recover")
                .to_vec(),
        ))
    }

    // todo: add auth
    async fn sizes_put_handler(
        new_sizes: Vec<ShirtSize>,
        state: ShirtSizeState,
        sender: broadcast::Sender<()>,
        bearer_validation: Arc<BearerValidation>,
        token: BearerToken,
    ) -> Result<(), Error> {
        bearer_validation.authorise(token)?;
        {
            let mut sizes = state
                .sizes
                .write()
                .expect("Poisoned lock for shirt sizes cannot continue");
            *sizes = new_sizes;
        }
        {
            state
                .users
                .write()
                .expect("Poisoned lock for shirt sizes cannot continue")
                .clear();
        }
        sender.send(())?;
        Ok(())
    }

    async fn login_handler(
        name: Name,
        sender: broadcast::Sender<()>,
        state: ShirtSizeState,
        mut session: WritableSession,
        bearer_validation: Arc<BearerValidation>,
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
            let mut users = state.users.write().expect("poisoned mutex, time to fail");
            users.insert(
                user_id.clone(),
                UserData {
                    is_admin,
                    name,
                    vote: None,
                },
            );
        };
        sender.send(())?;
        Ok(Json(is_admin))
    }

    fn get_votes(state: &ShirtSizeState, own_id: &UserId) -> Result<Votes, Error> {
        let visible = { *state.revealed.read().expect("Poisoned") };
        let votes = state.users.read().expect("Poisoned RwLock. Cannot procees");
        let own_vote = votes.get(own_id).map(|data| data.vote.clone()).flatten();
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

    async fn send_votes(
        state: &ShirtSizeState,
        send: &mut SplitSink<WebSocket, Message>,
        user_id: &UserId,
    ) -> Result<(), Error> {
        let votes_serialised = serde_json::to_string(&get_votes(state, user_id)?)?;
        send.send(Message::Text(votes_serialised)).await?;
        Ok(())
    }

    async fn votes_handler(
        state: ShirtSizeState,
        sender: broadcast::Sender<()>,
        session: ReadableSession,
        wsu: WebSocketUpgrade,
    ) -> Result<Response, Error> {
        let user_id = session
            .get::<UserId>("id")
            .ok_or_else(|| Error::Message("Failed to find a user id for current session".into()))?;
        let rx = sender.subscribe();
        let handle_socket = |ws: WebSocket| async move {
            info!("Opening web socket");
            let (mut sender, receiver) = ws.split();
            match send_votes(&state, &mut sender, &user_id).await {
                Ok(()) => {
                    tokio::spawn(read(state.clone(), receiver));
                    tokio::spawn(write(state, rx, sender, user_id.clone()));
                }
                Err(err) => warn!("Failed to get and send votes over new websocket: {err}"),
            }
        };
        Ok(wsu.on_upgrade(handle_socket))
    }

    async fn read(shirt_size_state: ShirtSizeState, mut receiver: SplitStream<WebSocket>) {
        while let Some(message) = receiver.next().await {
            match message {
                Ok(okay) => match okay {
                    Message::Text(_) => (),
                    Message::Binary(_) => (),
                    Message::Ping(_) => (),
                    Message::Pong(_) => (),
                    Message::Close(_) => {
                        info!("closing web-socket");
                        return;
                    }
                },
                Err(err) => warn!("Failed to read web socket: {err}"),
            }
        }
    }

    async fn write(
        shirt_size_state: ShirtSizeState,
        mut update: broadcast::Receiver<()>,
        mut sender: SplitSink<WebSocket, Message>,
        user_id: UserId,
    ) {
        while update.recv().await.is_ok() {
            if let Err(err) = send_votes(&shirt_size_state, &mut sender, &user_id).await {
                warn!("Error sending web-socket {err:?}");
            }
        }
    }

    async fn write_once(
        shirt_size_state: &ShirtSizeState,
        sender: &mut SplitSink<WebSocket, Message>,
    ) -> Result<(), Error> {
        let data = {
            let sizes = shirt_size_state.sizes.read().unwrap().clone();
            let users = shirt_size_state.users.read().unwrap().clone();
            let revealed = *shirt_size_state.revealed.read().unwrap();
            serde_json::to_string(&(sizes, users, revealed))?
        };
        let message = Message::Text(data);
        sender
            .send(message)
            .await
            .map_err(|err| Error::Message(format!("Failed to send {err:?}")))?;
        Ok(())
    }
}

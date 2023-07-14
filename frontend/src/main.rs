use std::collections::HashMap;
use std::rc::Rc;

use anyhow::Context;
use futures::stream;
use futures::{FutureExt, StreamExt};
use serde::de::DeserializeOwned;
use serde::Serialize;
use shirt_size_server::shirt_state::{Name, ShirtSize, Votes};
use sycamore::futures::spawn_local_scoped;
use sycamore::prelude::*;

use js_sys::{Object, Uint8Array};
use tracing::{info, warn};
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{Event, HtmlInputElement, ReadableStream, ReadableStreamDefaultReader};

fn set_key_signal(event: Event, key_signal: &Signal<String>) -> anyhow::Result<()> {
    let value = event
        .target()
        .ok_or_else(|| Error::Message("No target found".to_string()))?
        .dyn_into::<HtmlInputElement>()
        .map_err(|err| {
            Error::Message(format!(
                "Failed to find HtmlInputElement from event target: {err:?}"
            ))
        })?
        .value();
    key_signal.set(value);
    Ok(())
}

fn save_key<'a>(key_signal: &'a Signal<String>, edit_key: &'a Signal<bool>) -> anyhow::Result<()> {
    let key_rc = key_signal.get();
    set_local("key", key_rc.as_ref())?;
    edit_key.set(false);
    Ok(())
}
#[allow(non_snake_case)]
#[component]
fn KeyControl<'a, G: Html>(cx: Scope<'a>) -> View<G> {
    let edit_key = create_signal(cx, false);
    let current_key = match get_local::<String>("key") {
        Err(err) => {
            warn!("Failed to get current key: {err}");
            String::new()
        }
        Ok(Some(string)) => string,
        Ok(None) => String::new(),
    };
    let key_signal = create_signal(cx, current_key);
    view! { cx,
        ({
            if *edit_key.get() {
                view! {cx,
                    div {
                        input(on:input = move |event| if let Err(err) = set_key_signal(event, key_signal) {
                            warn!("Failed to set key in memory: {err}")
                        }, value=key_signal.get())
                    }
                    div(on:click=|_| if let Err(err) = save_key(key_signal, edit_key) {
                        warn!("Failed to save key to storage: {err}")
                    }) {"save key"}
                }
            } else {
                view! {cx,
                    div(on:click = |_| edit_key.set(true)) {"edit key"}
                }
            }
        })
    }
}
#[allow(non_snake_case)]
#[component]
async fn App<'a, G: Html>(ctx: Scope<'a>) -> View<G> {
    view! { ctx,
        KeyControl()
        hr {}
        div { PageComponent }
    }
}

#[derive(Debug, Clone)]
enum VotePageState {
    Voting {
        own_vote: Option<ShirtSize>,
        users: Vec<(Name, bool)>,
    },
    Voted {
        own_vote: Option<ShirtSize>,
        votes: Vec<(Name, Option<ShirtSize>)>,
    },
}

#[derive(Debug, Clone)]
enum PageState {
    NotLoggedIn,
    Error,
    VotePage {
        user: Name,
        shirt_sizes: Vec<ShirtSize>,
        admin: bool,
    },
}

trait JsValueToErr<T> {
    fn to_err(self) -> Result<T, Error>;
}

fn js_val_to_err(js_val: JsValue) -> Error {
    Error::Message(format!("{js_val:?}"))
}

impl<T> JsValueToErr<T> for Result<T, JsValue> {
    fn to_err(self) -> Result<T, Error> {
        self.map_err(js_val_to_err)
    }
}

async fn log_in(name: &Name) -> anyhow::Result<PageState> {
    let admin = reqwasm::http::Request::put("/api/v1/shirt-size/login")
        .header("Content-Type", "application/json")
        .body(serde_json::to_string(name)?)
        .set_key_header()
        .send()
        .await?
        .json::<bool>()
        .await?;
    set_local("name", name)?;

    let shirt_sizes = reqwasm::http::Request::get("/api/v1/shirt-size/sizes")
        .header("Content-Type", "application/json")
        .send()
        .await?
        .json::<Vec<ShirtSize>>()
        .await?;
    Ok(PageState::VotePage {
        user: name.clone(),
        shirt_sizes,
        admin,
    })
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Error: {0}")]
    Message(String),
}

trait LogError {
    fn log_error(self);
}

impl<T> LogError for anyhow::Result<T> {
    fn log_error(self) {
        if let Err(err) = self {
            warn!("received error: {err:?}");
        }
    }
}

fn get_local<T: DeserializeOwned>(key: &str) -> anyhow::Result<Option<T>> {
    let storage = web_sys::window()
        .ok_or_else(|| Error::Message("Failed to get window when running log_in".into()))?
        .local_storage()
        .to_err()?
        .ok_or_else(|| Error::Message("Failed to get local storage".into()))?;

    Ok(match storage.get_item(key).to_err()? {
        Some(json_str) => Some(serde_json::from_str::<T>(&json_str)?),
        None => None,
    })
}

fn set_local<T: Serialize>(key: &str, t: &T) -> anyhow::Result<()> {
    let storage = web_sys::window()
        .ok_or_else(|| Error::Message("Failed to get window when running log_in".into()))?
        .local_storage()
        .to_err()?
        .ok_or_else(|| Error::Message("Failed to get local storage".into()))?;
    let t_str = serde_json::to_string(t)?;

    Ok(storage.set_item(key, &t_str).to_err()?)
}

#[allow(non_snake_case)]
#[component]
async fn PageComponent<G: Html>(cx: Scope<'_>) -> View<G> {
    let (name, initial_state) = match get_local("name") {
        Err(err) => {
            warn!(
                "Now something is badly wrong. Try clearing local cookies/local storage: {err:?}"
            );
            (Name::new(String::new()), PageState::Error)
        }
        Ok(None) => (Name::new(String::new()), PageState::NotLoggedIn),
        Ok(Some(name)) => (name, PageState::NotLoggedIn),
    };
    let name = create_signal(cx, name);
    let page_state: RcSignal<PageState> = create_rc_signal(initial_state);
    view! {cx,  ({
        let page_state_clone = page_state.clone();
        let page_state_clone_2 = page_state.clone();
        match page_state.get().as_ref() {
            PageState::NotLoggedIn => view! { cx,
                LoginComponent((name, page_state_clone))
            },
            PageState::Error => {
                view! {cx, ({
                let page_state_clone = page_state_clone.clone();
                let mut is_error = true;
                spawn_local_scoped(cx, async move {
                    while is_error {
                        futures_timer::Delay::new(std::time::Duration::from_millis(300)).await;
                        let stored_name = name.get();
                        let new_state = match log_in(stored_name.as_ref()).await {
                            Ok(new_state) => {
                                is_error = false;
                                new_state
                            }
                            Err(err) => {
                                warn!("Failed to login: {err}");
                                PageState::Error
                            }
                        };
                        page_state_clone.set(new_state);
                    }
                });
                view! { cx,  div {"Error"} }
                })}
            }
            PageState::VotePage { user, shirt_sizes, admin } => view! {cx,
                VoteComponent((user.clone(), *admin, shirt_sizes.clone(), page_state_clone_2))
            },
        }
    })}
}

#[allow(non_snake_case)]
#[component]
async fn LoginComponent<'a, G: Html>(
    cx: Scope<'a>,
    input: (&'a Signal<Name>, RcSignal<PageState>),
) -> View<G> {
    let (name, page_state) = input;
    let change_name = move |event: Event| -> anyhow::Result<()> {
        let value = event
            .target()
            .ok_or_else(|| Error::Message("No target found".to_string()))?
            .dyn_into::<HtmlInputElement>()
            .map_err(|err| {
                Error::Message(format!(
                    "Failed to find HtmlInputElement from event target: {err:?}"
                ))
            })?
            .value();
        name.set(Name::new(value));
        Ok(())
    };
    let initial_name = name.get().as_str().to_string();
    view! {cx,
        div {
            input(
                type = "text",
                on:input = move |event| change_name(event).log_error(),
                value = initial_name,
            )

        }
        div(on:click = move |_| {
            let page_state_clone = page_state.clone();
            spawn_local_scoped(cx, async move {
                let name = name.get();
                let new_state = match log_in(name.as_ref()).await {
                    Ok(new_state) => new_state,
                    Err(err) => {
                        warn!("Failed to login: {err}");
                        PageState::Error
                    }
                };
                page_state_clone.set(new_state);
            });
        }) {"login"}
    }
}

fn vote_page_from_votes(votes: Votes) -> VotePageState {
    match votes {
        Votes::VotesHidden { own_vote, votes } => VotePageState::Voting {
            own_vote,
            users: votes,
        },
        Votes::VotesRevealed { own_vote, votes } => VotePageState::Voted { own_vote, votes },
    }
}

async fn vote(shirt_size: ShirtSize) -> anyhow::Result<()> {
    let body = serde_json::to_string(&shirt_size)?;
    reqwasm::http::Request::put("/api/v1/shirt-size/vote")
        .header("Content-Type", "application/json")
        .body(body)
        .send()
        .await?;
    Ok(())
}

fn count_votes(votes: &Vec<(Name, Option<ShirtSize>)>) -> Vec<(Option<ShirtSize>, usize)> {
    let mut return_map = HashMap::new();
    for (_, shirt_size) in votes {
        match return_map.get_mut(shirt_size) {
            Some(count) => *count += 1usize,
            None => {
                return_map.insert(shirt_size.clone(), 1usize);
            }
        }
    }
    return_map.into_iter().collect()
}

async fn reset() -> anyhow::Result<()> {
    reqwasm::http::Request::get("/api/v1/shirt-size/reset")
        .header("Content-Type", "application/json")
        .set_key_header()
        .send()
        .await?;
    Ok(())
}
async fn reveal() -> anyhow::Result<()> {
    reqwasm::http::Request::get("/api/v1/shirt-size/reveal")
        .header("Content-Type", "application/json")
        .set_key_header()
        .send()
        .await?;
    Ok(())
}

#[allow(non_snake_case)]
#[component]
async fn VoteComponent<'a, G: Html>(
    cx: Scope<'a>,
    page_state: (Name, bool, Vec<ShirtSize>, RcSignal<PageState>),
) -> View<G> {
    let (name, is_admin, shirt_sizes, page_state) = page_state;
    let vote_state = create_rc_signal(VotePageState::Voting {
        own_vote: None,
        users: vec![],
    });
    let current_vote = create_signal(cx, None);
    let revealed = create_signal(cx, false);

    let page_state_for_handler = page_state.clone();
    let vote_state_for_handler = vote_state.clone();
    let event_source_stream: ReadableStream =
        reqwasm::http::Request::get("/api/v1/shirt-size/votes-sse")
            .send()
            .await
            .expect("Failed to get eventsource")
            .body()
            .expect("What up with this response body");
    let default_reader = event_source_stream
        .get_reader()
        .dyn_into::<ReadableStreamDefaultReader>()
        .expect("Okay");

    let event_stream = || {
        Box::pin(stream::unfold(false, move |is_done| {
            let promise = default_reader.clone().read();
            async move {
                if !is_done {
                    let res = wasm_bindgen_futures::JsFuture::from(promise)
                        .map(|value_res| -> Result<(Votes, bool), anyhow::Error> {
                            let value = value_res.map_err(|err| {
                                Error::Message(format!(
                                    "failed to get next event in event source:{err:?}"
                                ))
                            })?;
                            let props = value.dyn_into::<Object>().map_err(|err| {
                                Error::Message(format!(
                                    "failed to cast to message event as object: {err:?}"
                                ))
                            })?;
                            let byte_array =
                                js_sys::Reflect::get(&props, &JsValue::from_str("value"))
                                    .map_err(|err| {
                                        Error::Message(format!("failed to get value : {err:?}"))
                                    })?
                                    .dyn_into::<Uint8Array>()
                                    .map_err(|err| {
                                        Error::Message(format!(
                                            "failed to cast data in byte array: {err:?}"
                                        ))
                                    })?;
                            let done = js_sys::Reflect::get(&props, &JsValue::from_str("done"))
                                .map_err(|err| {
                                    Error::Message(format!("failed to get done: {err:?}"))
                                })?
                                .as_bool()
                                .ok_or_else(|| {
                                    Error::Message("second element of object is not a bool".into())
                                })?;
                            let string = String::from_utf8(byte_array.to_vec())?;
                            if string.len() < 5 {
                                return Err(Error::Message(format!(
                                    "Received a message event without \"data\" field: {string}"
                                ))
                                .into());
                            }
                            let (data_key, data) = string.split_at(5);
                            if data_key == "data:" {
                                let votes =
                                    serde_json::from_str::<Votes>(data).context(format!(
                                "Not votes in this sucky ass event source json?!?!?: {data}",
                            ))?;
                                Ok((votes, done))
                            } else {
                                Err(Error::Message(format!(
                                    "event source didn't return any data: {data_key}, {data}"
                                ))
                                .into())
                            }
                        })
                        .await;
                    match res {
                        Ok((votes, done)) => Some((votes, done)),
                        Err(err) => {
                            warn!("Failure in event source: {err}");
                            None
                        }
                    }
                } else {
                    warn!("Closing sse!");
                    None
                }
            }
        }))
    };

    let mut restarting_stream =
        Box::pin(stream::repeat(event_stream).flat_map(|inner_stream| inner_stream()));

    spawn_local_scoped(cx, async move {
        while let Some(votes) = restarting_stream.next().await {
            info!("Votes: {votes:?}");
            if votes.find_name(&name) {
                vote_state_for_handler.set(vote_page_from_votes(votes));
            } else {
                page_state_for_handler.set(PageState::NotLoggedIn)
            }
        }
        page_state_for_handler.set(PageState::Error)
    });

    let reset_clos = move |_| {
        spawn_local_scoped(cx, async move {
            if let Err(err) = reset().await {
                warn!("Failed to reset votes: {err}");
            }
        });
    };
    let reveal_clos = move |_| {
        spawn_local_scoped(cx, async move {
            if let Err(err) = reveal().await {
                warn!("Failed to reveal votes: {err}");
            }
        });
    };
    view! { cx,
        div(on:click = move |_| page_state.set(PageState::NotLoggedIn)) {"logout"}
        h3 {"Voting Page"}
        ShirtSizes((shirt_sizes.clone(), is_admin, revealed, current_vote))
        hr {}
        ({
            match vote_state.get().as_ref() {
                VotePageState::Voted { own_vote, votes } => {
                    current_vote.set(own_vote.clone());
                    revealed.set(true);
                    let counts = create_signal(cx, count_votes(votes));
                    let votes_signal = create_signal(cx, votes.clone());
                    view! { cx,
                        ul {
                            Indexed(
                                iterable = counts,
                                view = |cx, (vote, count)| view! {cx,
                                    li { (
                                        match vote.clone() {
                                            Some(shirt_size) => view! { cx,
                                                (format!("Shirt size: {} – {count}", shirt_size.as_str()))

                                            },
                                            None => view! {cx,
                                                (format!("? – {count}"))
                                            },
                                        }
                                    ) }
                                }
                            )
                        }
                        hr {}
                        ul {
                            Indexed(
                                iterable = votes_signal,
                                view = |cx, (name, vote)| view! {cx,
                                    li {(match vote.clone() {
                                        Some(shirt_size) => format!("{} – shirt size: {}", name.as_str(), shirt_size.as_str()),
                                        None => format!("{} – ?", name.as_str()),
                                    })}
                                }
                            )
                        }
                        hr {}
                        (
                            if is_admin { view! {cx,
                                div(on:click = reset_clos) {"reset"}
                            }} else {
                                view! {cx, div {}}
                            }
                        )
                    }
                }
                VotePageState::Voting { own_vote, users } => {
                    current_vote.set(own_vote.clone());
                    revealed.set(false);
                    let users_signal = create_signal(cx, users.clone());
                    view! { cx,
                        ul {
                            Indexed(
                                iterable = users_signal,
                                view = |cx, (name, vote)| view! {cx,
                                    li {(format!("{}, {}", name.as_str(), if vote {"v"} else {"?"}))}
                                }
                            )
                        }
                        hr {}
                        (
                            if is_admin { view! {cx,
                                div(on:click = reveal_clos) {"reveal"}
                            }} else {
                                view! {cx, div {}}
                            }
                        )
                    }
                }

            }
        })
    }
}

#[allow(non_snake_case)]
#[component]
fn ShirtSizeComponent<'a, G: Html>(
    cx: Scope<'a>,
    data: (
        &'a Signal<ShirtSize>,
        &'a Signal<bool>, // editing
        &'a Signal<Option<ShirtSize>>,
        &'a Signal<bool>, // revealed
    ),
) -> View<G> {
    let (shirt_size, editing, current_vote, revealed) = data;

    let vote_closure = move |_| {
        let shirt_size_clone = shirt_size.get().as_ref().clone();
        spawn_local_scoped(cx, async move {
            if let Err(err) = vote(shirt_size_clone).await {
                warn!("Failed to cast vote: {err}")
            }
        })
    };

    let set_closure = move |event: web_sys::Event| -> anyhow::Result<()> {
        let value = event
            .target()
            .ok_or_else(|| Error::Message("No target found".to_string()))?
            .dyn_into::<HtmlInputElement>()
            .map_err(|err| {
                Error::Message(format!(
                    "Failed to find HtmlInputElement from event target: {err:?}"
                ))
            })?
            .value();
        shirt_size.set(ShirtSize::new(value));
        Ok(())
    };

    view! {cx, ({
        let current_vote = current_vote.get();
        if *editing.get() {
           view! {cx,
                input(on:input = move |event| {
                    if let Err(err) = set_closure(event) {
                        warn!("Failed to set shirt_size: {err}")
                    }
                }, value=shirt_size.get().as_str())
            }
        } else {
            let current_size = shirt_size.get().as_ref().clone() ;
            let revealed = *revealed.get();
            match current_vote.as_ref() {
                Some(current_vote) if current_vote == &current_size => view! {cx,
                    div { b {(format!("Size: {}", current_size.as_str()))} }
                },
                _ if revealed => view! {cx,
                    div {(format!("Size: {}", shirt_size.get().as_str()))}
                },
                _ => view! {cx,
                    div(on:click = vote_closure) {(format!("Size: {}", shirt_size.get().as_str()))}
                },
            }
        }
    })}
}

trait SetKeyHeader
where
    Self: Sized,
{
    fn set_key_header(self) -> Self;
}

const NO_KEY: &str = "no_key";

impl SetKeyHeader for reqwasm::http::Request {
    fn set_key_header(self) -> Self {
        let key = match get_local::<String>("key") {
            Ok(Some(key)) if !key.is_empty() => key,
            Ok(Some(_)) => {
                info!("Empty key stored, defaulting to \"{NO_KEY}\"");
                NO_KEY.into()
            }
            Ok(None) => {
                info!("No key stored, defaulting to \"{NO_KEY}\"");
                NO_KEY.into()
            }
            Err(err) => {
                warn!("Failed to get key from local storage, defaulting to \"{NO_KEY}\": {err}");
                NO_KEY.into()
            }
        };
        self.header("Authorization", &format!("Bearer {key}"))
    }
}

async fn save_sizes(sizes: &Signal<Vec<&Signal<ShirtSize>>>) -> anyhow::Result<()> {
    let sizes = sizes
        .get()
        .iter()
        .map(|size| size.get().as_ref().clone())
        .collect::<Vec<_>>();
    let body = serde_json::to_string(&sizes)?;
    reqwasm::http::Request::put("/api/v1/shirt-size/sizes")
        .header("Content-Type", "application/json")
        .set_key_header()
        .body(body)
        .send()
        .await?;
    Ok(())
}

#[allow(non_snake_case)]
#[component]
fn ShirtSizes<'a, G: Html>(
    cx: Scope<'a>,
    shirt_sizes_and_is_admin: (
        Vec<ShirtSize>,
        bool,
        &'a Signal<bool>,
        &'a Signal<Option<ShirtSize>>,
    ),
) -> View<G> {
    let (shirt_sizes, is_admin, revealed, current_vote) = shirt_sizes_and_is_admin;
    let shirt_sizes = create_signal(
        cx,
        shirt_sizes
            .into_iter()
            .map(|size| create_signal(cx, size))
            .collect::<Vec<_>>(),
    );
    let editing = create_signal(cx, false);
    let add_size = move |shirt_sizes_rc: Rc<Vec<&'a Signal<ShirtSize>>>| {
        info!("clicked");
        let mut shirt_sizes_vec = shirt_sizes_rc.to_vec();
        let new_size = create_signal(cx, ShirtSize::new("O".into()));
        shirt_sizes_vec.push(new_size);
        shirt_sizes.set(shirt_sizes_vec)
    };
    view! { cx,
        Indexed(
            iterable = shirt_sizes,
            view = move |cx, shirt_size| {
                view! {cx,
                    ShirtSizeComponent((shirt_size, editing, current_vote, revealed))
                }
            }
        )
        ({
            if is_admin {
                let is_editing = editing.get();
                let shirt_sizes_rc = shirt_sizes.get();
                if *is_editing {
                    view! { cx,
                        div(on:click = move|_| add_size(shirt_sizes_rc.clone())) {"+"}
                        div(on:click = move |_| spawn_local_scoped(cx, async move {
                            if let Err(err) = save_sizes(shirt_sizes).await {
                                warn!("Failed to save shirt sizes: {err}");
                            }
                            editing.set(false);
                        })){"Save"}
                    }
                } else {
                    view! { cx,
                        div(on:click = |_| {
                            editing.set(true);
                        }) {"Edit"}
                    }
                }
            } else {
                view! {cx, div {}}
            }
        })
    }
}

fn main() {
    console_error_panic_hook::set_once();

    tracing_wasm::set_as_global_default();
    info!("Test");

    sycamore::render(|cx| {
        let page_state = create_signal(cx, PageState::NotLoggedIn);
        view! { cx,
            App
        }
    });
}

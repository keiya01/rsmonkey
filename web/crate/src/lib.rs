use wasm_bindgen::prelude::*;
use yew::prelude::*;

mod components;

#[wasm_bindgen(start)]
pub fn run() {
    App::<components::editor::Editor>::new().mount_to_body();
}

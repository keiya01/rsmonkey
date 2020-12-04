use std::rc::Rc;
use std::cell::RefCell;
use wasm_bindgen::prelude::*;
use yew::prelude::*;
use yew::web_sys::HtmlTextAreaElement;
use evaluator::environment::Environment;

fn exec(buf: String, env: &mut Rc<RefCell<Environment>>) -> String {
    let l = lexer::Lexer::new(buf);
    let mut p = parser::Parser::new(l);
    let program = p.parse_program();

    if p.errors.len() > 0 {
        return "unexpected error occurred".to_string();
    }

    format!("{}", evaluator::eval(program, env))
}

struct Header {
    link: ComponentLink<Self>,
}

impl Component for Model {
    type Message = ();
    type Properties = ();
    fn create(_: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self { link }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        false
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        // Should only return "true" if new properties are different to
        // previously received properties.
        // This component has no properties so we will always return "false".
        false
    }

    fn view(&self) -> Html {
        html! {
            <header>
                <h1>The rsmonkey Playground</h1>
            </header>
        }
    }
}

#[wasm_bindgen(start)]
pub fn run() {
    App::<Model>::new().mount_to_body();
}

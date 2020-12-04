use std::rc::Rc;
use std::cell::RefCell;
use wasm_bindgen::prelude::*;
use yew::prelude::*;
use yew::web_sys::HtmlTextAreaElement;
use interpreter::{lexer, parser, evaluator};
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

struct Model {
    link: ComponentLink<Self>,
    textarea: NodeRef,
    result: String,
}

enum Msg {
    Run,
}

impl Component for Model {
    type Message = Msg;
    type Properties = ();
    fn create(_: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link,
            textarea: NodeRef::default(),
            result: String::new(),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::Run => {
                let elm = match self.textarea.cast::<HtmlTextAreaElement>() {
                    Some(elm) => elm,
                    None => return false,
                };
                let mut env = Environment::new();
                self.result = exec(elm.value(), &mut env);
            }
        }
        true
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        // Should only return "true" if new properties are different to
        // previously received properties.
        // This component has no properties so we will always return "false".
        false
    }

    fn view(&self) -> Html {
        html! {
            <div>
                <textarea ref=self.textarea.clone()></textarea>
                <button onclick=self.link.callback(|_| Msg::Run)>{ "Run" }</button>
                <div>{ format!("Result: {}", &self.result) }</div>
            </div>
        }
    }
}

#[wasm_bindgen(start)]
pub fn run() {
    App::<Model>::new().mount_to_body();
}

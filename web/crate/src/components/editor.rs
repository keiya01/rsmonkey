use std::rc::Rc;
use std::cell::RefCell;
use yew::prelude::*;
use yew::web_sys::HtmlTextAreaElement;
use interpreter::{lexer, parser, evaluator};
use evaluator::environment::Environment;

use super::header::Header;

fn exec(buf: String, env: &mut Rc<RefCell<Environment>>) -> String {
    let l = lexer::Lexer::new(buf);
    let mut p = parser::Parser::new(l);
    let program = p.parse_program();

    if p.errors.len() > 0 {
        return "unexpected error occurred".to_string();
    }

    format!("{}", evaluator::eval(program, env))
}

pub struct Editor {
    link: ComponentLink<Self>,
    textarea: NodeRef,
    result: String,
}

pub enum Msg {
    Run,
}

impl Component for Editor {
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
        let default_value = "let count = fn(x) {
  if(x > 10) {
    return x;
  }
  count(x + 1);
};
count(0);
";
        html! {
            <>
                <Header on_click=self.link.callback(|_| Msg::Run) />
                <textarea class="editor__area" ref=self.textarea.clone()>{ default_value }</textarea>
                <div class="editor__result-block">
                  <span class="editor__result-block-title">{ "Result:" }</span>
                  { &self.result }
                </div>
            </>
        }
    }
}

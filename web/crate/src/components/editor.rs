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

struct State {
  lines: usize,
  result: String,
}

pub struct Editor {
    link: ComponentLink<Self>,
    state: State,
    textarea: NodeRef,
    default_value: String,
}

pub enum Msg {
    Run,
    NewLine(String),
}

impl Component for Editor {
    type Message = Msg;
    type Properties = ();
    fn create(_: Self::Properties, link: ComponentLink<Self>) -> Self {
      let default_value = "let count = fn(x) {
  if(x > 10) {
    return x;
  }
  count(x + 1);
};
count(0);
";
        let state = State {
          lines: default_value.lines().collect::<Vec<&str>>().len() + 1,
          result: "".into(),
        };
        Self {
            link,
            state,
            textarea: NodeRef::default(),
            default_value: default_value.into(),
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
                self.state.result = exec(elm.value(), &mut env);
            },
            Msg::NewLine(val) => {
              self.state.lines = val.lines().collect::<Vec<&str>>().len() + 1;
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
            <>
                <Header on_click=self.link.callback(|_| Msg::Run) />
                <div class="editor__area-block">
                  <div class="editor__area-line">
                    { 
                      for (0..self.state.lines).collect::<Vec<usize>>().iter().map(|i| {
                        html! { <span class="editor__area-line-text">{ i + 1 }</span> }
                      })
                    }
                  </div>
                  <textarea
                    class="editor__area"
                    oninput=self.link.callback(|e: InputData| Msg::NewLine(e.value))
                    ref=self.textarea.clone()
                  >
                    { &self.default_value }
                  </textarea>
                </div>
                <div class="editor__result-block">
                  <span class="editor__result-block-title">{ "Result:" }</span>
                  { &self.state.result }
                </div>
            </>
        }
    }
}

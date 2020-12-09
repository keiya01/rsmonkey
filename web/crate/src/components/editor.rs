use std::rc::Rc;
use std::cell::RefCell;
use yew::prelude::*;
use yew::web_sys::HtmlTextAreaElement;
use interpreter::{lexer, parser, evaluator};
use evaluator::environment::Environment;
use evaluator::{builtins, object};

use super::header::Header;

fn exec(buf: String, env: &mut Rc<RefCell<Environment>>) -> Rc<RefCell<Vec<String>>> {
    let l = lexer::Lexer::new(buf);
    let mut p = parser::Parser::new(l);
    let program = p.parse_program();

    if p.errors.len() > 0 {
      return Rc::new(RefCell::new(p.errors));
    }

    let outputs = Rc::new(RefCell::new(vec![]));
    
    let f = Rc::new(
      RefCell::new({
        let outputs = Rc::clone(&outputs);
        move |args| {
          for v in args {
            let mut outputs = outputs.borrow_mut();
            outputs.push(format!("{}", v));
          }
          return object::Object::Null;
        }
      })
    );

    env.borrow_mut().set("puts", object::Object::External(object::External::new(f)));

    let evaluated = evaluator::eval(program, env);

    outputs.borrow_mut().push(format!("{}", evaluated));
    
    outputs
}

fn count_lines(s: &str) -> usize {
  s.chars().filter(|c| c == &'\n').count() + 1
}

struct State {
  lines: usize,
  result: Rc<RefCell<Vec<String>>>,
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
      let default_value = "let map = fn(arr, f) {
  let iter = fn(arr, acc) {
    if(len(arr) == 0) {
      acc
    } else {
      iter(rest(arr), push(acc, f(first(arr))));
    }
  };
  iter(arr, []);
}

map([1, 2, 3], fn(v) { v + 1 });
";
        let state = State {
          lines: count_lines(default_value),
          result: Rc::new(RefCell::new(vec![])),
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
                let mut env = Environment::new(builtins::new_builtins());
                self.state.result = exec(elm.value(), &mut env);
            },
            Msg::NewLine(val) => {
              self.state.lines = count_lines(&val);
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
                  <div>
                    { for self.state.result.borrow().iter().map(|val| html! { <><span>{ &val }</span><br/></> }) }
                  </div>
                </div>
            </>
        }
    }
}

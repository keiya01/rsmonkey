use yew::prelude::*;

#[derive(Clone, PartialEq, Properties)]
pub struct Props {
    pub on_click: Callback<MouseEvent>,
}

pub struct Header {
    pub props: Props,
}

impl Component for Header {
    type Message = ();
    type Properties = Props;
    fn create(props: Self::Properties, _: ComponentLink<Self>) -> Self {
        Self { props }
    }

    fn update(&mut self, _msg: Self::Message) -> ShouldRender {
        unimplemented!()
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        // Should only return "true" if new properties are different to
        // previously received properties.
        // This component has no properties so we will always return "false".
        false
    }

    fn view(&self) -> Html {
        html! {
            <header class="header__block">
                <div class="header__block-left">
                    <h1 class="header__block-title">{ "The rsmonkey Playground" }</h1>
                    <button class="header__block--font-size header__run-button" onclick=self.props.on_click.clone()>{ "RUN" }</button>
                </div>
                <div class="header__block-right">
                    <a class="header__block-link header__block--font-size" href="https://github.com/keiya01/rsmonkey">{ "Github" }</a>
                </div>
            </header>
        }
    }
}

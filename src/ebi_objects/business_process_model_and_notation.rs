use std::rc::Rc;

pub struct BusinessProcessModelAndNotation {
    pub pools: Vec<Pool>,
}

pub enum Pool {
    BlackBox,
    WhiteBox(Vec<Rc<Element>>),
}

#[derive(Clone)]
pub enum Element {
    Gateway(Gateway),
    Task,
    Event(Event),
}

#[derive(Clone, Copy)]
pub enum Gateway {
    Exclusive,
    Parallel,
    Inclusive,
    EventBased,
}

#[derive(Clone, Copy)]
pub enum Event {
    Start,
    StartMessage,
    End,
    EndMessage,
    CatchingMessage,
    ThrowingMessage,
}

#[derive(Clone)]
pub enum Flow {
    Sequence {
        source: Rc<Element>,
        target: Rc<Element>,
    },
    Message {
        source: Rc<Element>,
        target: Rc<Element>,
    },
}

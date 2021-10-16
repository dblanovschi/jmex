use std::{
    fs::File,
    io::{Read, Stdin},
};

pub enum Input {
    Stdin(Stdin),
    File(File),
}

impl Input {
    fn input(&mut self) -> &mut dyn Read {
        match self {
            Input::Stdin(stdin) => stdin,
            Input::File(f) => f,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("io error")]
    IoError(#[from] std::io::Error),
}

#[derive(Debug, Clone, Copy)]
struct TextPos(usize);

#[derive(Debug, Clone, Copy)]
struct ArrayIndex(usize);

#[derive(Debug)]
enum ParserBranchFragment {
    Object(TextPos),
    Property(TextPos, String),

    Array(TextPos),
    ArrayElement(TextPos, ArrayIndex),
}

impl ParserBranchFragment {
    /// Returns `true` if the parser branch fragment is [`Object`].
    ///
    /// [`Object`]: ParserBranchFragment::Object
    fn is_object(&self) -> bool {
        matches!(self, Self::Object(..))
    }

    /// Returns `true` if the parser branch fragment is [`Property`].
    ///
    /// [`Property`]: ParserBranchFragment::Property
    fn is_property(&self) -> bool {
        matches!(self, Self::Property(..))
    }

    /// Returns `true` if the parser branch fragment is [`Array`].
    ///
    /// [`Array`]: ParserBranchFragment::Array
    fn is_array(&self) -> bool {
        matches!(self, Self::Array(..))
    }

    /// Returns `true` if the parser branch fragment is [`ArrayElement`].
    ///
    /// [`ArrayElement`]: ParserBranchFragment::ArrayElement
    fn is_array_element(&self) -> bool {
        matches!(self, Self::ArrayElement(..))
    }

    fn as_object(&self) -> Option<&TextPos> {
        if let Self::Object(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn as_array(&self) -> Option<&TextPos> {
        if let Self::Array(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn try_into_object(self) -> Result<TextPos, Self> {
        if let Self::Object(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    fn try_into_array(self) -> Result<TextPos, Self> {
        if let Self::Array(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

#[derive(Debug)]
pub struct ParserBranch {
    fragments: Vec<ParserBranchFragment>,
}

impl ParserBranch {
    fn push_object(&mut self, tp: TextPos) {
        self.fragments.push(ParserBranchFragment::Object(tp));
    }

    fn push_property(&mut self, tp: TextPos, property: String) {
        self.fragments
            .push(ParserBranchFragment::Property(tp, property));
    }

    fn push_array(&mut self, tp: TextPos) {
        self.fragments.push(ParserBranchFragment::Array(tp));
    }

    fn push_array_element(&mut self, tp: TextPos, index: ArrayIndex) {
        self.fragments
            .push(ParserBranchFragment::ArrayElement(tp, index));
    }

    fn pop_object(&mut self) -> Result<TextPos, PopError> {
        self.fragments
            .pop()
            .ok_or(PopError::NoElement)?
            .try_into_object()
            .map(|tp| {
                self.pop_unimportant();
                tp
            })
            .map_err(|it| {
                self.fragments.push(it);
                PopError::NotProperType
            })
    }

    fn pop_array(&mut self) -> Result<TextPos, PopError> {
        self.fragments
            .pop()
            .ok_or(PopError::NoElement)?
            .try_into_array()
            .map(|tp| {
                self.pop_unimportant();
                tp
            })
            .map_err(|it| {
                self.fragments.push(it);
                PopError::NotProperType
            })
    }

    fn pop_unimportant(&mut self) {
        while let Some(
            ParserBranchFragment::ArrayElement(_, _) | ParserBranchFragment::Property(_, _),
        ) = self.fragments.last()
        {
            self.fragments.pop();
        }
    }

    pub fn enclosing_context(&self) -> Option<EnclosingContext> {
        self.fragments
            .iter()
            .filter(|it| it.is_array() || it.is_object())
            .last()
            .map(|f| match f {
                ParserBranchFragment::Object(_) => EnclosingContext::Object,
                ParserBranchFragment::Array(_) => EnclosingContext::Array,
                ParserBranchFragment::Property(_, _) | ParserBranchFragment::ArrayElement(_, _) => {
                    unreachable!()
                }
            })
    }
}

pub enum EnclosingContext {
    Object,
    Array,
}

#[derive(Debug)]
enum PopError {
    NoElement,
    NotProperType,
}

#[cfg(feature = "small-buf")]
const BUFFER_SIZE: usize = 4;

#[cfg(not(feature = "small-buf"))]
const BUFFER_SIZE: usize = 16 * 8092;

struct ParserState<'a> {
    current_branch: ParserBranch,

    input: &'a mut Input,
    pos: usize,
    size: usize,
    buf: [u8; BUFFER_SIZE],

    begin_offset: usize,
}

impl<'a> ParserState<'a> {
    fn bump(&mut self) {
        self.pos += 1;

        if self.pos == self.size {
            self.begin_offset += self.size;

            self.pos = 0;
            self.size = self.input.input().read(&mut self.buf).expect("IO error");

            if self.size == 0 {
                panic!("Unexpecte EOF");
            }
        }
    }

    fn current(&self) -> u8 {
        self.buf[self.pos]
    }

    fn at_byte(&self, b: u8) -> bool {
        self.current() == b
    }

    fn at(&self, ch: char) -> bool {
        debug_assert_eq!(ch.len_utf8(), 1);

        self.at_byte(ch as u8)
    }

    fn expect_byte(&mut self, b: u8) {
        assert_eq!(self.current(), b);

        self.bump();
    }

    fn expect(&mut self, ch: char) {
        debug_assert_eq!(ch.len_utf8(), 1);

        self.expect_byte(ch as u8);
    }

    fn current_text_offset(&self) -> TextPos {
        TextPos(self.begin_offset + self.pos)
    }
}

pub enum VisitorAction {
    Recurse,
    Continue,
}

pub enum VisitStrStrategy {
    RawWhole,
    ParsedWhole,
    RawChunked,
}

pub trait Visitor {
    fn visit_object(&mut self, branch: &ParserBranch) -> VisitorAction {
        VisitorAction::Recurse
    }

    fn visit_object_end(&mut self, branch: &ParserBranch) {}

    fn visit_property(&mut self, branch: &ParserBranch) -> VisitorAction {
        VisitorAction::Recurse
    }

    fn visit_property_end(&mut self, branch: &ParserBranch) {}

    fn visit_array(&mut self, branch: &ParserBranch) -> VisitorAction {
        VisitorAction::Recurse
    }

    fn visit_array_end(&mut self, branch: &ParserBranch) {}

    fn visit_array_element(&mut self, branch: &ParserBranch) -> VisitorAction {
        VisitorAction::Recurse
    }

    fn visit_number(&mut self, branch: &ParserBranch, num: f64) {}
    fn visit_bool(&mut self, branch: &ParserBranch, b: bool) {}
    fn visit_null(&mut self, branch: &ParserBranch) {}

    fn visit_str_strategy(&mut self, branch: &ParserBranch) -> Option<VisitStrStrategy> {
        None
    }

    fn visit_str_chunks_begin(&mut self, branch: &ParserBranch) {}
    fn visit_str_chunk(&mut self, branch: &ParserBranch, chunk: &[u8]) {}
    fn visit_str_chunks_end(&mut self, branch: &ParserBranch) {}

    fn visit_str(&mut self, branch: &ParserBranch, s: String) {}
}

type OptVis<'a> = Option<&'a mut dyn Visitor>;

fn parse_value(state: &mut ParserState, vis: OptVis) {
    if state.at('{') {
        parse_object(state, vis);
    }
}

fn parse_object(state: &mut ParserState, vis: OptVis) {
    let tp = state.current_text_offset();
    state.expect('{');

    state.current_branch.push_object(tp);
    skip_ws(state);

    match vis {
        Some(vis) => match vis.visit_object(&state.current_branch) {
            VisitorAction::Recurse => {
                parse_object_props(state, Some(vis));

                state.expect('}');

                vis.visit_object_end(&state.current_branch);
            }
            VisitorAction::Continue => {
                parse_object_props(state, None);

                state.expect('}');
            }
        },
        None => {
            parse_object_props(state, None);

            state.expect('}');
        }
    }

    state
        .current_branch
        .pop_object()
        .expect("Something wrong happened");
}

fn parse_object_props(state: &mut ParserState, vis: OptVis) {
    skip_ws(state);

    match vis {
        Some(vis) => {
            while !state.at('}') {
                parse_property(state, Some(vis));

                skip_ws(state);

                if !state.at('}') {
                    state.expect(',');
                    skip_ws(state);
                }
            }
        }
        None => {
            while !state.at('}') {
                parse_property(state, None);

                skip_ws(state);

                if !state.at('}') {
                    state.expect(',');
                    skip_ws(state);
                }
            }
        }
    }
}

fn parse_property(state: &mut ParserState, vis: OptVis) {
    let tp = state.current_text_offset();
    let p = parse_property_name(state);

    state.current_branch.push_property(tp, p);

    skip_ws(state);
    state.expect(':');

    skip_ws(state);

    match vis {
        Some(vis) => match vis.visit_property(&state.current_branch) {
            VisitorAction::Recurse => {
                parse_value(state, Some(vis));
            }
            VisitorAction::Continue => {
                parse_value(state, None);
            }
        },
        None => {}
    }

    state.current_branch.pop_unimportant();
}

fn parse_property_name(state: &mut ParserState) -> String {
    if state.at('"') {
        parse_double_quote_whole_parsed_str(state)
    } else if state.at('\'') {
        parse_single_quote_whole_parsed_str(state)
    } else {
        parse_ident(state)
    }
}

fn parse_double_quote_whole_parsed_str(state: &mut ParserState) -> String {
    state.expect('"');

    let mut contents = Vec::<u8>::with_capacity(32);
    let mut escaped = false;

    // TODO: \n, \uXXXX and similar

    loop {
        if state.at('"') {
            if escaped {
                contents.push('"' as u8);
            } else {
                state.bump();
                break String::from_utf8(contents).expect("UTF8");
            }
        } else {
            let ch = state.current();
            if ch == '\\' as u8 {
                if escaped {
                    contents.push('\\' as u8);
                }

                escaped = !escaped;
            } else {
                contents.push(ch)
            }
        }

        state.bump();
    }
}

fn parse_single_quote_whole_parsed_str(state: &mut ParserState) -> String {
    state.expect('\'');

    let mut contents = Vec::<u8>::with_capacity(32);
    let mut escaped = false;

    // TODO: \n, \uXXXX and similar

    loop {
        if state.at('\'') {
            if escaped {
                contents.push('\'' as u8);
            } else {
                state.bump();
                break String::from_utf8(contents).expect("UTF8");
            }
        } else {
            let ch = state.current();
            if ch == '\\' as u8 {
                if escaped {
                    contents.push('\\' as u8);
                }

                escaped = !escaped;
            } else {
                contents.push(ch)
            }
        }

        state.bump();
    }
}

fn parse_ident(state: &mut ParserState) -> String {
    let mut contents = Vec::<u8>::with_capacity(32);

    loop {
        let ch = state.current();
        if (ch as char).is_whitespace() || ch == ':' as u8 {
            break String::from_utf8(contents).expect("UTF8");
        } else if "$".contains(ch as char) || (ch as char).is_ascii_alphanumeric() {
            contents.push(ch)
        } else {
            panic!("Invalid char in property name: {}", ch as char)
        }

        state.bump();
    }
}

fn skip_ws(state: &mut ParserState) {
    while (state.current() as char).is_whitespace() {
        state.bump();
    }
}

pub fn run(mut input: Input, exe: String, vis: &mut dyn Visitor) -> Result<(), Error> {
    let mut buf = [0; BUFFER_SIZE];

    let size = input.input().read(&mut buf).expect("IO error");

    let mut state = ParserState {
        current_branch: ParserBranch { fragments: vec![] },
        input: &mut input,
        pos: 0,
        size,
        buf,
        begin_offset: 0,
    };

    parse_object(&mut state, Some(vis));

    Ok(())
}

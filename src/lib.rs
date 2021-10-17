use std::{
    convert::TryInto,
    fs::File,
    io::{Cursor, Read, Stdin},
};

use ansi_term::Color;

pub enum Input {
    Stdin(Stdin),
    File(File),
    String(Cursor<Vec<u8>>),
}

impl Input {
    pub fn from_string(s: String) -> Self {
        let c = Cursor::new(s.into_bytes());
        Self::String(c)
    }

    pub fn from_stdin(s: Stdin) -> Self {
        Self::Stdin(s)
    }

    pub fn from_file(f: File) -> Self {
        Self::File(f)
    }

    fn input(&mut self) -> &mut dyn Read {
        match self {
            Self::Stdin(stdin) => stdin,
            Self::File(f) => f,
            Self::String(s) => s,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("io error")]
    IoError(#[from] std::io::Error),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct TextPos(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ArrayIndex(usize);

impl From<usize> for ArrayIndex {
    fn from(idx: usize) -> Self {
        Self(idx)
    }
}

#[derive(Debug)]
enum ParserBranchFragment {
    Object(TextPos),
    Property(TextPos, String),

    Array(TextPos),
    ArrayElement(TextPos, ArrayIndex),
}

#[allow(dead_code)]
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
    fn r(&self) -> ParserBranchRef {
        ParserBranchRef {
            fragments: &self.fragments,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ParserBranchRef<'a> {
    fragments: &'a [ParserBranchFragment],
}

impl<'a> From<&'a ParserBranch> for ParserBranchRef<'a> {
    fn from(b: &'a ParserBranch) -> Self {
        Self {
            fragments: &b.fragments,
        }
    }
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
            .map_err(|it| {
                self.fragments.push(it);
                PopError::NotProperType
            })
    }

    fn pop_property(&mut self) -> Result<(), PopError> {
        match self.fragments.last() {
            Some(ParserBranchFragment::Property(_, _)) => {
                self.fragments.pop();
                Ok(())
            }
            Some(_) => Err(PopError::NotProperType),
            None => Err(PopError::NoElement),
        }
    }

    fn pop_array_element(&mut self) -> Result<(), PopError> {
        match self.fragments.last() {
            Some(ParserBranchFragment::ArrayElement(_, _)) => {
                self.fragments.pop();
                Ok(())
            }
            Some(_) => Err(PopError::NotProperType),
            None => Err(PopError::NoElement),
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
const BUFFER_SIZE: usize = 1;

#[cfg(not(feature = "small-buf"))]
const BUFFER_SIZE: usize = 16 * 8092;

struct ParserState<'a> {
    current_branch: ParserBranch,

    input: &'a mut Input,
    pos: usize,
    size: usize,
    buf: Vec<u8>,

    begin_offset: usize,
}

impl<'a> ParserState<'a> {
    fn bump(&mut self) {
        self.pos += 1;

        if self.pos == self.size {
            self.begin_offset += self.size;

            self.pos = 0;
            self.size = self.input.input().read(&mut self.buf).expect("IO error");
        }
    }

    fn at_eof(&self) -> bool {
        self.pos == 0 && self.size == 0
    }

    fn current(&self) -> u8 {
        if self.at_eof() {
            panic!("Unexpected EOF");
        }

        self.buf[self.pos]
    }

    fn at(&self, b: u8) -> bool {
        self.current() == b
    }

    fn expect(&mut self, b: u8) {
        assert_eq!(self.current(), b);

        self.bump();
    }

    // Safety: kw is valid UTF8
    fn expect_from_kw(&mut self, b: u8, kw: &[u8]) {
        let current = self.current();
        assert_eq!(
            current,
            b,
            "Expecting {} (from keyword {}), got: {}",
            b as char,
            unsafe { std::str::from_utf8_unchecked(kw) },
            current as char
        );

        self.bump();
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

#[allow(unused_variables)]
pub trait Visitor {
    fn visit_object(&mut self, branch: ParserBranchRef) -> VisitorAction {
        VisitorAction::Recurse
    }

    fn visit_object_end(&mut self, branch: ParserBranchRef) {}

    fn visit_property(&mut self, branch: ParserBranchRef) -> VisitorAction {
        VisitorAction::Recurse
    }

    fn visit_property_end(&mut self, branch: ParserBranchRef) {}

    fn visit_array(&mut self, branch: ParserBranchRef) -> VisitorAction {
        VisitorAction::Recurse
    }

    fn visit_array_end(&mut self, branch: ParserBranchRef) {}

    fn visit_array_element(&mut self, branch: ParserBranchRef) -> VisitorAction {
        VisitorAction::Recurse
    }

    fn visit_array_element_end(&mut self, branch: ParserBranchRef) {}

    fn visit_number(&mut self, branch: ParserBranchRef, num: f64) {}
    fn visit_bool(&mut self, branch: ParserBranchRef, b: bool) {}
    fn visit_null(&mut self, branch: ParserBranchRef) {}

    fn visit_str_strategy(&mut self, branch: ParserBranchRef) -> Option<VisitStrStrategy> {
        None
    }

    fn visit_str_chunks_begin(&mut self, branch: ParserBranchRef) {}
    fn visit_str_chunk(&mut self, branch: ParserBranchRef, chunk: &[u8]) {}
    fn visit_str_chunks_end(&mut self, branch: ParserBranchRef) {}

    fn visit_str(&mut self, branch: ParserBranchRef, s: String) {}

    fn visit_root_end(&mut self) {}
}

type OptVis<'a> = Option<&'a mut dyn Visitor>;

fn parse_value(state: &mut ParserState, vis: OptVis) {
    let ch = state.current();

    match ch {
        b'{' => parse_object(state, vis),
        b'[' => parse_array(state, vis),

        b't' | b'f' => try_parse_bool(state, vis),
        b'n' => try_parse_null(state, vis),

        b'\'' => match vis {
            Some(vis) => match vis.visit_str_strategy(state.current_branch.r().into()) {
                Some(VisitStrStrategy::ParsedWhole) => {
                    let s = parse_single_quote_whole_parsed_str(state);
                    vis.visit_str(state.current_branch.r().into(), s);
                }
                Some(VisitStrStrategy::RawChunked) => todo!(),
                Some(VisitStrStrategy::RawWhole) => todo!(),
                None => eat_single_quote_str(state),
            },
            None => eat_single_quote_str(state),
        },
        b'"' => match vis {
            Some(vis) => match vis.visit_str_strategy(state.current_branch.r()) {
                Some(VisitStrStrategy::ParsedWhole) => {
                    let s = parse_double_quote_whole_parsed_str(state);
                    vis.visit_str(state.current_branch.r(), s);
                }
                Some(VisitStrStrategy::RawChunked) => todo!(),
                Some(VisitStrStrategy::RawWhole) => todo!(),
                None => eat_double_quote_str(state),
            },
            None => eat_double_quote_str(state),
        },

        b'0'..=b'9' | b'-' => {
            parse_number(state, vis);
        }

        _ => unreachable!(),
    }
}

// Safety: kw is valid UTF8
fn try_parse_kw(state: &mut ParserState, kw: &[u8]) {
    for b in kw {
        state.expect_from_kw(*b, kw);
    }

    if !state.at_eof() {
        let c = state.current();
        if !c.is_ascii_whitespace() && c != b',' && c != b']' && c != b'}' && c != b':' {
            panic!(
                "Unexpected char after keyword {}: {}",
                unsafe { std::str::from_utf8_unchecked(kw) },
                c
            )
        }
    }
}

fn parse_number(state: &mut ParserState, vis: OptVis) {
    let sign = if state.at(b'-') {
        state.bump();
        -1.
    } else {
        1.
    };

    let whole = parse_int(state);

    let frac = if !state.at_eof() && state.at(b'.') {
        state.bump();

        parse_frac(state)
    } else {
        0.
    };

    let exp = if !state.at_eof() && (state.at(b'E') || state.at(b'e')) {
        state.bump();

        let sign = if state.at(b'-') {
            state.bump();
            -1_i64
        } else if state.at(b'+') {
            state.bump();
            1
        } else {
            1
        };

        let num = parse_int(state);

        num * sign
    } else {
        0
    };

    if let Some(vis) = vis {
        let num = sign * (whole as f64 + frac) * 10_f64.powi(exp.try_into().unwrap());
        vis.visit_number(state.current_branch.r(), num);
    }
}

fn parse_int(state: &mut ParserState) -> i64 {
    let mut num = 0;

    while !state.at_eof() && (b'0'..=b'9').contains(&state.current()) {
        let c = state.current();
        state.bump();

        num = num * 10 + (c - b'0') as i64;
    }

    num
}

fn parse_frac(state: &mut ParserState) -> f64 {
    let mut p = 0.1;
    let mut f = 0.;

    while !state.at_eof() && (b'0'..=b'9').contains(&state.current()) {
        let c = state.current();
        state.bump();

        f += ((c - b'0') as f64) * p;

        p /= 10.;
    }

    f
}

fn try_parse_null(state: &mut ParserState, vis: OptVis) {
    try_parse_kw(state, b"null");

    if let Some(vis) = vis {
        vis.visit_null(state.current_branch.r());
    }
}

fn try_parse_bool(state: &mut ParserState, vis: Option<&mut dyn Visitor>) {
    if state.at(b't') {
        try_parse_kw(state, b"true");

        if let Some(vis) = vis {
            vis.visit_bool(state.current_branch.r(), true);
        }
    } else if state.at(b'f') {
        try_parse_kw(state, b"false");

        if let Some(vis) = vis {
            vis.visit_bool(state.current_branch.r(), false);
        }
    } else {
        unreachable!()
    }
}

fn parse_object(state: &mut ParserState, vis: OptVis) {
    let tp = state.current_text_offset();
    state.expect(b'{');

    skip_ws(state);

    match vis {
        Some(vis) => {
            state.current_branch.push_object(tp);

            match vis.visit_object(state.current_branch.r()) {
                VisitorAction::Recurse => {
                    parse_object_props(state, Some(vis));

                    state.expect(b'}');

                    vis.visit_object_end(state.current_branch.r());
                }
                VisitorAction::Continue => {
                    parse_object_props(state, None);

                    state.expect(b'}');
                }
            }

            state
                .current_branch
                .pop_object()
                .expect("Something wrong happened");
        }
        None => {
            parse_object_props(state, None);

            state.expect(b'}');
        }
    }
}

fn parse_array(state: &mut ParserState, vis: OptVis) {
    let tp = state.current_text_offset();
    state.expect(b'[');

    skip_ws(state);

    match vis {
        Some(vis) => {
            state.current_branch.push_array(tp);

            match vis.visit_array(state.current_branch.r()) {
                VisitorAction::Recurse => {
                    parse_array_elements(state, Some(vis));

                    state.expect(b']');

                    vis.visit_array_end(state.current_branch.r());
                }
                VisitorAction::Continue => {
                    parse_array_elements(state, None);

                    state.expect(b']');
                }
            }

            state
                .current_branch
                .pop_array()
                .expect("Something wrong happened");
        }
        None => {
            parse_array_elements(state, None);

            state.expect(b']');
        }
    }
}

fn parse_array_elements(state: &mut ParserState, vis: OptVis) {
    skip_ws(state);

    let mut idx = 0;

    match vis {
        Some(vis) => {
            while !state.at(b']') {
                parse_array_element(state, Some(vis), idx);

                skip_ws(state);

                if !state.at(b']') {
                    state.expect(b',');
                    skip_ws(state);
                }

                idx += 1;
            }
        }
        None => {
            while !state.at(b']') {
                parse_array_element(state, None, idx);

                skip_ws(state);

                if !state.at(b']') {
                    state.expect(b',');
                    skip_ws(state);
                }

                idx += 1;
            }
        }
    }
}

fn parse_array_element(state: &mut ParserState, vis: OptVis, elem_idx: usize) {
    skip_ws(state);

    let tp = state.current_text_offset();

    match vis {
        Some(vis) => {
            state
                .current_branch
                .push_array_element(tp, ArrayIndex(elem_idx));

            match vis.visit_array_element(state.current_branch.r()) {
                VisitorAction::Recurse => {
                    parse_value(state, Some(vis));

                    vis.visit_array_element_end(state.current_branch.r());
                }
                VisitorAction::Continue => {
                    parse_value(state, None);
                }
            }

            state.current_branch.pop_array_element().unwrap();
        }
        None => {
            parse_value(state, None);
        }
    }
}

fn parse_object_props(state: &mut ParserState, vis: OptVis) {
    skip_ws(state);

    match vis {
        Some(vis) => {
            while !state.at(b'}') {
                parse_property(state, Some(vis));

                skip_ws(state);

                if !state.at(b'}') {
                    state.expect(b',');
                    skip_ws(state);
                }
            }
        }
        None => {
            while !state.at(b'}') {
                parse_property(state, None);

                skip_ws(state);

                if !state.at(b'}') {
                    state.expect(b',');
                    skip_ws(state);
                }
            }
        }
    }
}

fn parse_property(state: &mut ParserState, vis: OptVis) {
    match vis {
        Some(vis) => {
            let tp = state.current_text_offset();
            let p = parse_property_name(state);

            state.current_branch.push_property(tp, p);

            skip_ws(state);
            state.expect(b':');

            skip_ws(state);

            match vis.visit_property(state.current_branch.r()) {
                VisitorAction::Recurse => {
                    parse_value(state, Some(vis));

                    vis.visit_property_end(state.current_branch.r());
                }
                VisitorAction::Continue => {
                    parse_value(state, None);
                }
            }

            state.current_branch.pop_property().unwrap();
        }
        None => {
            eat_property_name(state);

            skip_ws(state);
            state.expect(b':');

            skip_ws(state);

            parse_value(state, None);
        }
    }
}

fn parse_property_name(state: &mut ParserState) -> String {
    if state.at(b'"') {
        parse_double_quote_whole_parsed_str(state)
    } else if state.at(b'\'') {
        parse_single_quote_whole_parsed_str(state)
    } else {
        parse_ident(state)
    }
}

fn eat_property_name(state: &mut ParserState) {
    if state.at(b'"') {
        eat_double_quote_str(state)
    } else if state.at(b'\'') {
        eat_single_quote_str(state)
    } else {
        eat_ident(state)
    }
}

fn parse_double_quote_whole_parsed_str(state: &mut ParserState) -> String {
    state.expect(b'"');

    let mut contents = Vec::<u8>::with_capacity(32);
    let mut escaped = false;

    // TODO: \n, \uXXXX and similar

    loop {
        if state.at(b'"') {
            if escaped {
                contents.push(b'"');
            } else {
                state.bump();
                break unsafe { String::from_utf8_unchecked(contents) };
            }
        } else {
            let ch = state.current();
            if ch == b'\\' {
                if escaped {
                    contents.push(b'\\');
                }

                escaped = !escaped;
            } else {
                contents.push(ch);

                escaped = false;
            }
        }

        state.bump();
    }
}

fn parse_single_quote_whole_parsed_str(state: &mut ParserState) -> String {
    state.expect(b'\'');

    let mut contents = Vec::<u8>::with_capacity(32);
    let mut escaped = false;

    // TODO: \n, \uXXXX and similar

    loop {
        if state.at(b'\'') {
            if escaped {
                contents.push(b'\'');
            } else {
                state.bump();
                break unsafe { String::from_utf8_unchecked(contents) };
            }
        } else {
            let ch = state.current();
            if ch == b'\\' {
                if escaped {
                    contents.push(b'\\');
                }

                escaped = !escaped;
            } else {
                contents.push(ch);

                escaped = false;
            }
        }

        state.bump();
    }
}

fn parse_ident(state: &mut ParserState) -> String {
    let mut contents = Vec::<u8>::with_capacity(32);

    loop {
        let ch = state.current();
        if ch.is_ascii_whitespace() || ch == b':' {
            break unsafe { String::from_utf8_unchecked(contents) };
        } else if b"$".contains(&ch) || ch.is_ascii_alphanumeric() {
            contents.push(ch)
        } else {
            panic!("Invalid char in property name: {}", ch as char)
        }

        state.bump();
    }
}

fn eat_double_quote_str(state: &mut ParserState) {
    state.expect(b'"');

    let mut escaped = false;

    // TODO: \n, \uXXXX and similar

    loop {
        if state.at(b'"') {
            if escaped {
            } else {
                state.bump();
                break;
            }
        } else {
            let ch = state.current();
            if ch == b'\\' {
                escaped = !escaped;
            } else {
                escaped = false;
            }
        }

        state.bump();
    }
}

fn eat_single_quote_str(state: &mut ParserState) {
    state.expect(b'\'');

    let mut escaped = false;

    // TODO: \n, \uXXXX and similar

    loop {
        if state.at(b'\'') {
            if escaped {
            } else {
                state.bump();
                break;
            }
        } else {
            let ch = state.current();
            if ch == b'\\' {
                escaped = !escaped;
            } else {
                escaped = false;
            }
        }

        state.bump();
    }
}

fn eat_ident(state: &mut ParserState) {
    loop {
        let ch = state.current();
        if ch.is_ascii_whitespace() || ch == b':' {
            break;
        } else if b"$".contains(&ch) || ch.is_ascii_alphanumeric() {
        } else {
            panic!("Invalid char in property name: {}", ch as char)
        }

        state.bump();
    }
}

fn skip_ws(state: &mut ParserState) {
    while !state.at_eof() && state.current().is_ascii_whitespace() {
        state.bump();
    }
}

pub fn run(mut input: Input, _exe: String, vis: &mut dyn Visitor) -> Result<(), Error> {
    let mut buf = vec![0; BUFFER_SIZE];

    let size = input.input().read(&mut buf).expect("IO error");

    let mut state = ParserState {
        current_branch: ParserBranch { fragments: vec![] },
        input: &mut input,
        pos: 0,
        size,
        buf,
        begin_offset: 0,
    };

    skip_ws(&mut state);
    parse_value(&mut state, Some(vis));

    vis.visit_root_end();

    Ok(())
}

pub struct DoNothingVis;

#[allow(unused_variables)]
impl Visitor for DoNothingVis {
    fn visit_object(&mut self, branch: ParserBranchRef) -> VisitorAction {
        VisitorAction::Continue
    }

    fn visit_object_end(&mut self, branch: ParserBranchRef) {}

    fn visit_property(&mut self, branch: ParserBranchRef) -> VisitorAction {
        VisitorAction::Continue
    }

    fn visit_property_end(&mut self, branch: ParserBranchRef) {}

    fn visit_array(&mut self, branch: ParserBranchRef) -> VisitorAction {
        VisitorAction::Continue
    }

    fn visit_array_end(&mut self, branch: ParserBranchRef) {}

    fn visit_array_element(&mut self, branch: ParserBranchRef) -> VisitorAction {
        VisitorAction::Continue
    }

    fn visit_array_element_end(&mut self, branch: ParserBranchRef) {}

    fn visit_number(&mut self, branch: ParserBranchRef, num: f64) {}

    fn visit_bool(&mut self, branch: ParserBranchRef, b: bool) {}

    fn visit_null(&mut self, branch: ParserBranchRef) {}

    fn visit_str_strategy(&mut self, branch: ParserBranchRef) -> Option<VisitStrStrategy> {
        None
    }

    fn visit_str_chunks_begin(&mut self, branch: ParserBranchRef) {}

    fn visit_str_chunk(&mut self, branch: ParserBranchRef, chunk: &[u8]) {}

    fn visit_str_chunks_end(&mut self, branch: ParserBranchRef) {}

    fn visit_str(&mut self, branch: ParserBranchRef, s: String) {}
}

pub enum Filter {
    ArrayElement(ArrayIndex),
    PropertyName(String),
}

pub struct SquashFilterVis<V: Visitor>(V, Filter);

impl<V: Visitor> SquashFilterVis<V> {
    pub fn new(v: V, f: Filter) -> Self {
        Self(v, f)
    }
}

impl<V: Visitor> Visitor for SquashFilterVis<V> {
    fn visit_object(&mut self, branch: ParserBranchRef) -> VisitorAction {
        if self.1.matches(branch) {
            self.0.visit_object(self.1.squash(branch))
        } else {
            VisitorAction::Continue
        }
    }

    fn visit_object_end(&mut self, branch: ParserBranchRef) {
        if self.1.matches(branch) {
            self.0.visit_object_end(self.1.squash(branch))
        }
    }

    fn visit_property(&mut self, branch: ParserBranchRef) -> VisitorAction {
        if self.1.matches(branch) {
            self.0.visit_property(self.1.squash(branch))
        } else {
            VisitorAction::Continue
        }
    }

    fn visit_property_end(&mut self, branch: ParserBranchRef) {
        if self.1.matches(branch) {
            self.0.visit_property_end(self.1.squash(branch))
        }
    }

    fn visit_array(&mut self, branch: ParserBranchRef) -> VisitorAction {
        if self.1.matches(branch) {
            self.0.visit_array(self.1.squash(branch))
        } else {
            VisitorAction::Continue
        }
    }

    fn visit_array_end(&mut self, branch: ParserBranchRef) {
        if self.1.matches(branch) {
            self.0.visit_array_end(self.1.squash(branch))
        }
    }

    fn visit_array_element(&mut self, branch: ParserBranchRef) -> VisitorAction {
        if self.1.matches(branch) {
            self.0.visit_array_element(self.1.squash(branch))
        } else {
            VisitorAction::Continue
        }
    }

    fn visit_array_element_end(&mut self, branch: ParserBranchRef) {
        if self.1.matches(branch) {
            self.0.visit_array_element_end(self.1.squash(branch))
        }
    }

    fn visit_number(&mut self, branch: ParserBranchRef, num: f64) {
        if self.1.matches(branch) {
            self.0.visit_number(self.1.squash(branch), num)
        }
    }

    fn visit_bool(&mut self, branch: ParserBranchRef, b: bool) {
        if self.1.matches(branch) {
            self.0.visit_bool(self.1.squash(branch), b)
        }
    }

    fn visit_null(&mut self, branch: ParserBranchRef) {
        if self.1.matches(branch) {
            self.0.visit_null(self.1.squash(branch))
        }
    }

    fn visit_str_strategy(&mut self, branch: ParserBranchRef) -> Option<VisitStrStrategy> {
        if self.1.matches(branch) {
            self.0.visit_str_strategy(self.1.squash(branch))
        } else {
            None
        }
    }

    fn visit_str_chunks_begin(&mut self, branch: ParserBranchRef) {
        if self.1.matches(branch) {
            self.0.visit_str_chunks_begin(self.1.squash(branch))
        }
    }

    fn visit_str_chunk(&mut self, branch: ParserBranchRef, chunk: &[u8]) {
        if self.1.matches(branch) {
            self.0.visit_str_chunk(self.1.squash(branch), chunk)
        }
    }

    fn visit_str_chunks_end(&mut self, branch: ParserBranchRef) {
        if self.1.matches(branch) {
            self.0.visit_str_chunks_end(self.1.squash(branch))
        }
    }

    fn visit_str(&mut self, branch: ParserBranchRef, s: String) {
        if self.1.matches(branch) {
            self.0.visit_str(self.1.squash(branch), s)
        }
    }

    fn visit_root_end(&mut self) {
        self.0.visit_root_end()
    }
}

impl Filter {
    fn matches(&self, branch: ParserBranchRef) -> bool {
        let frag = branch
            .fragments
            .iter()
            .filter(|it| it.is_property() || it.is_array_element())
            .next();
        let frag = match frag {
            Some(frag) => frag,
            None => return true, // root value, always match
        };
        match self {
            Filter::ArrayElement(filter_idx) => match frag {
                ParserBranchFragment::Object(_)
                | ParserBranchFragment::Property(_, _)
                | ParserBranchFragment::Array(_) => false,
                ParserBranchFragment::ArrayElement(_, idx) => filter_idx == idx,
            },
            Filter::PropertyName(filter_prop_name) => match frag {
                ParserBranchFragment::Property(_, name) => filter_prop_name == name,
                ParserBranchFragment::Object(_)
                | ParserBranchFragment::Array(_)
                | ParserBranchFragment::ArrayElement(_, _) => false,
            },
        }
    }

    fn squash<'a>(&self, branch: ParserBranchRef<'a>) -> ParserBranchRef<'a> {
        branch.skip(2)
    }
}

impl<'a> ParserBranchRef<'a> {
    fn skip(&self, n: usize) -> Self {
        Self {
            fragments: &self.fragments[n.min(self.fragments.len())..],
        }
    }
}

pub struct PrintVisitor {
    need_lf: bool,
    need_comma: bool,
    ansi: bool,
}

fn print_depth(branch: ParserBranchRef) -> usize {
    branch
        .fragments
        .iter()
        .filter(|it| it.is_array() || it.is_object())
        .count()
        * 4
}

fn print_depth_m4(branch: ParserBranchRef) -> usize {
    let d = branch
        .fragments
        .iter()
        .filter(|it| it.is_array() || it.is_object())
        .count()
        * 4;

    if d == 0 {
        0
    } else {
        d - 4
    }
}

impl PrintVisitor {
    pub fn new(ansi: bool) -> Self {
        Self {
            need_lf: false,
            need_comma: false,
            ansi,
        }
    }
}

#[allow(unused_variables)]
impl Visitor for PrintVisitor {
    fn visit_object(&mut self, branch: ParserBranchRef) -> VisitorAction {
        if self.ansi {
            println!("{}", Color::White.bold().paint("{"));
        } else {
            println!("{{");
        }

        VisitorAction::Recurse
    }

    fn visit_object_end(&mut self, branch: ParserBranchRef) {
        self.need_comma = false;

        if self.need_lf {
            println!();
            self.need_lf = false;
        }

        let d = print_depth_m4(branch);

        if self.ansi {
            print!("{:ident$}{}", "", Color::White.bold().paint("}"), ident = d);
        } else {
            print!("{:ident$}}}", "", ident = d);
        }
    }

    fn visit_property(&mut self, branch: ParserBranchRef) -> VisitorAction {
        if self.need_comma {
            print!(",");
            self.need_comma = false;
        }
        if self.need_lf {
            println!();
            self.need_lf = false;
        }

        let d = print_depth(branch);

        match branch.fragments.last().unwrap() {
            ParserBranchFragment::Property(_, name) => {
                if self.ansi {
                    print!(
                        r#"{:ident$}{name}{colon} "#,
                        "",
                        name = Color::Blue.bold().paint(format!(r#""{}""#, name)),
                        colon = Color::White.bold().paint(":"),
                        ident = d
                    );
                } else {
                    print!(r#"{:ident$}"{}": "#, "", name, ident = d);
                }
            }
            _ => unreachable!(),
        }

        VisitorAction::Recurse
    }

    fn visit_property_end(&mut self, branch: ParserBranchRef) {
        self.need_comma = true;
        self.need_lf = true;
    }

    fn visit_array(&mut self, branch: ParserBranchRef) -> VisitorAction {
        if self.ansi {
            println!("{}", Color::White.bold().paint("["))
        } else {
            println!("[");
        }
        VisitorAction::Recurse
    }

    fn visit_array_end(&mut self, branch: ParserBranchRef) {
        self.need_comma = false;

        if self.need_lf {
            println!();
            self.need_lf = false;
        }

        let d = print_depth_m4(branch);
        if self.ansi {
            print!("{:ident$}{}", "", Color::White.bold().paint("]"), ident = d);
        } else {
            print!("{:ident$}]", "", ident = d);
        }
    }

    fn visit_array_element(&mut self, branch: ParserBranchRef) -> VisitorAction {
        if self.need_comma {
            print!(",");
            self.need_comma = false;
        }
        if self.need_lf {
            println!();
            self.need_lf = false;
        }

        let d = print_depth(branch);
        print!("{:ident$}", "", ident = d);

        VisitorAction::Recurse
    }

    fn visit_array_element_end(&mut self, branch: ParserBranchRef) {
        self.need_comma = true;
        self.need_lf = true;
    }

    fn visit_number(&mut self, branch: ParserBranchRef, num: f64) {
        if self.ansi {
            print!("{}", Color::Yellow.paint(format!("{}", num)));
        } else {
            print!("{}", num);
        }
    }

    fn visit_bool(&mut self, branch: ParserBranchRef, b: bool) {
        if self.ansi {
            print!("{}", Color::Cyan.bold().paint(format!("{}", b)));
        } else {
            print!("{}", b);
        }
    }

    fn visit_null(&mut self, branch: ParserBranchRef) {
        if self.ansi {
            print!("{}", Color::Red.bold().paint("null"));
        } else {
            print!("null");
        }
    }

    fn visit_str_strategy(&mut self, branch: ParserBranchRef) -> Option<VisitStrStrategy> {
        Some(VisitStrStrategy::ParsedWhole)
    }

    fn visit_str_chunks_begin(&mut self, branch: ParserBranchRef) {}

    fn visit_str_chunk(&mut self, branch: ParserBranchRef, chunk: &[u8]) {}

    fn visit_str_chunks_end(&mut self, branch: ParserBranchRef) {}

    fn visit_str(&mut self, branch: ParserBranchRef, s: String) {
        if self.ansi {
            print!("{}", Color::Green.paint(format!("{:?}", s)));
        } else {
            print!("{:?}", s);
        }
    }

    fn visit_root_end(&mut self) {
        println!()
    }
}

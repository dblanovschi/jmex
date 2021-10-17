use jmex::Input;

fn main() {
    jmex::run(Input::from_string(r#"0"#.into()), "".into(), &mut Vis).unwrap();
    jmex::run(
        Input::from_string(r#"10000000000000000"#.into()),
        "".into(),
        &mut Vis,
    )
    .unwrap();
    jmex::run(Input::from_string(r#"0.1"#.into()), "".into(), &mut Vis).unwrap();
    jmex::run(Input::from_string(r#"0.1E10"#.into()), "".into(), &mut Vis).unwrap();
    jmex::run(Input::from_string(r#"0."#.into()), "".into(), &mut Vis).unwrap();
}

struct Vis;

impl jmex::Visitor for Vis {
    fn visit_object(&mut self, branch: &jmex::ParserBranch) -> jmex::VisitorAction {
        println!("Visiting object at {:?}", branch);
        jmex::VisitorAction::Recurse
    }

    fn visit_object_end(&mut self, branch: &jmex::ParserBranch) {
        println!("Visited object at {:?}", branch);
    }

    fn visit_property(&mut self, branch: &jmex::ParserBranch) -> jmex::VisitorAction {
        println!("Visiting property at {:?}", branch);
        jmex::VisitorAction::Recurse
    }

    fn visit_property_end(&mut self, branch: &jmex::ParserBranch) {
        println!("Visited property at {:?}", branch);
    }

    fn visit_array(&mut self, branch: &jmex::ParserBranch) -> jmex::VisitorAction {
        println!("Visiting array at {:?}", branch);

        jmex::VisitorAction::Recurse
    }

    fn visit_array_end(&mut self, branch: &jmex::ParserBranch) {
        println!("Visited array at {:?}", branch);
    }

    fn visit_array_element(&mut self, branch: &jmex::ParserBranch) -> jmex::VisitorAction {
        println!("Visiting array element at {:?}", branch);

        jmex::VisitorAction::Recurse
    }

    fn visit_array_element_end(&mut self, branch: &jmex::ParserBranch) {
        println!("Visited array element at {:?}", branch);
    }

    fn visit_number(&mut self, branch: &jmex::ParserBranch, num: f64) {
        println!("[{:?}]: Num: {}", branch, num);
    }

    fn visit_bool(&mut self, branch: &jmex::ParserBranch, b: bool) {
        println!("[{:?}]: Bool: {}", branch, b);
    }

    fn visit_null(&mut self, branch: &jmex::ParserBranch) {
        println!("[{:?}]: null", branch);
    }

    fn visit_str_strategy(
        &mut self,
        branch: &jmex::ParserBranch,
    ) -> Option<jmex::VisitStrStrategy> {
        None
    }

    fn visit_str_chunks_begin(&mut self, branch: &jmex::ParserBranch) {}

    fn visit_str_chunk(&mut self, branch: &jmex::ParserBranch, chunk: &[u8]) {}

    fn visit_str_chunks_end(&mut self, branch: &jmex::ParserBranch) {}

    fn visit_str(&mut self, branch: &jmex::ParserBranch, s: String) {}
}

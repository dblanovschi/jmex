use std::{error::Error, fs::File, path::PathBuf};

use clap::Clap;
use jmex::{Input, ParserBranchRef, PrintVisitor, VisitStrStrategy};

#[derive(Clap, Debug)]
pub struct App {
    /// Code to execute
    #[clap(default_value = ".")]
    exe: String,
    /// File to read input from, or stdin if not specified
    file: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let app: App = App::parse();

    let input = match app.file.as_ref() {
        Some(f) => Input::File(File::open(f)?),
        None => Input::Stdin(std::io::stdin()),
    };

    struct Vis;

    #[allow(unused_variables)]
    impl jmex::Visitor for Vis {
        fn visit_object(&mut self, branch: ParserBranchRef) -> jmex::VisitorAction {
            println!("Visiting object at {:?}", branch);
            jmex::VisitorAction::Recurse
        }

        fn visit_object_end(&mut self, branch: ParserBranchRef) {
            println!("Visited object at {:?}", branch);
        }

        fn visit_property(&mut self, branch: ParserBranchRef) -> jmex::VisitorAction {
            println!("Visiting property at {:?}", branch);
            jmex::VisitorAction::Recurse
        }

        fn visit_property_end(&mut self, branch: ParserBranchRef) {
            println!("Visited property at {:?}", branch);
        }

        fn visit_array(&mut self, branch: ParserBranchRef) -> jmex::VisitorAction {
            println!("Visiting array at {:?}", branch);

            jmex::VisitorAction::Recurse
        }

        fn visit_array_end(&mut self, branch: ParserBranchRef) {
            println!("Visited array at {:?}", branch);
        }

        fn visit_array_element(&mut self, branch: ParserBranchRef) -> jmex::VisitorAction {
            println!("Visiting array element at {:?}", branch);

            jmex::VisitorAction::Recurse
        }

        fn visit_array_element_end(&mut self, branch: ParserBranchRef) {
            println!("Visited array element at {:?}", branch);
        }

        fn visit_number(&mut self, branch: ParserBranchRef, num: f64) {
            println!("[{:?}]: Num: {}", branch, num);
        }

        fn visit_bool(&mut self, branch: ParserBranchRef, b: bool) {
            println!("[{:?}]: Bool: {}", branch, b);
        }

        fn visit_null(&mut self, branch: ParserBranchRef) {
            println!("[{:?}]: null", branch);
        }

        fn visit_str_strategy(&mut self, branch: ParserBranchRef) -> Option<VisitStrStrategy> {
            Some(VisitStrStrategy::ParsedWhole)
        }

        fn visit_str_chunks_begin(&mut self, branch: ParserBranchRef) {}

        fn visit_str_chunk(&mut self, branch: ParserBranchRef, chunk: &[u8]) {}

        fn visit_str_chunks_end(&mut self, branch: ParserBranchRef) {}

        fn visit_str(&mut self, branch: ParserBranchRef, s: String) {
            println!("[{:?}]: String: {:?}", branch, s);
        }
    }

    jmex::run(input, app.exe, &mut PrintVisitor::new(atty::is(atty::Stream::Stdout)))?;

    Ok(())
}

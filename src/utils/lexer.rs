use pest::iterators::Pairs;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "src/rslogo.pest"]
struct RsLogoParser;

//Parse the RSLogo text.
pub fn lexical_analyzer(file_content: &str) -> Result<Pairs<Rule>, String> {
    RsLogoParser::parse(Rule::file, file_content).map_err(|e| format!("{}", e))
}

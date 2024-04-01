use crate::utils::ast_definition::{Ast, AstNode, NodeType};
use crate::utils::lexer::Rule;
use pest::iterators::{Pair, Pairs};

// Generate an abstract syntax tree from the results of RSLogo's text parsing.
pub fn ast_build<'a>(ast: &mut Ast<'a>, pairs: Pairs<'a, Rule>) {
    let mut root = AstNode::new(NodeType::Root);

    for pair in pairs {
        root.add_child(depth_first_traversal(pair));
    }
    ast.root = root;
}

// Recursively traversing the results of text parsing.
fn depth_first_traversal(pair: Pair<Rule>) -> AstNode {
    let node_type = match pair.as_rule() {
        Rule::control_unary => NodeType::ControlUnary(pair.as_span()),
        Rule::control_float => NodeType::ControlFloat(pair.as_span()),
        Rule::control_int => NodeType::ControlInt(pair.as_span()),

        Rule::make => NodeType::Make(pair.as_span()),
        Rule::addassign => NodeType::AddAssign(pair.as_span()),
        Rule::variable_declaration => NodeType::VariableDeclaration(pair.as_span()),

        Rule::condition => NodeType::Condition(pair.as_span()),
        Rule::loop_lg => NodeType::Loop(pair.as_span()),
        Rule::macro_lg => NodeType::Macro(pair.as_span()),
        Rule::segment => NodeType::Segment(pair.as_span()),
        Rule::macro_name => NodeType::MacroName(pair.as_span()),
        Rule::parameter_list => NodeType::ParameterList(pair.as_span()),
        Rule::invocation => NodeType::Invocation(pair.as_span()),
        Rule::macro_segment => NodeType::MacroSegment(pair.as_span()),

        Rule::expression => NodeType::Expression(pair.as_span()),
        Rule::value => NodeType::Value(pair.as_span()),
        Rule::variable => NodeType::Variable(pair.as_span()),
        Rule::operator => NodeType::Operator(pair.as_span()),
        Rule::queries => NodeType::Queries(pair.as_span()),

        Rule::EOI => NodeType::Eoi(pair.as_span()),
        _ => panic!(),
    };
    let mut new_node = AstNode::new(node_type);
    for inner_pair in pair.into_inner() {
        new_node.add_child(depth_first_traversal(inner_pair));
    }
    new_node
}

use pest::Span;

//abstract syntax tree
pub struct Ast<'a> {
    pub logo_codes: String, //TODO Storing the RSLogo text is used to provide additional error information
    pub root: AstNode<'a>,
}

impl<'a> Ast<'a> {
    pub fn new(file_content: &'a str) -> Self {
        let logo_codes = String::from(file_content);
        let root = AstNode::new(NodeType::Root);
        Ast { logo_codes, root }
    }
}

#[derive(Debug)]
pub struct AstNode<'a> {
    pub node_type: NodeType<'a>,
    pub children: Vec<AstNode<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum NodeType<'a> {
    Root,
    ControlUnary(Span<'a>),
    ControlFloat(Span<'a>),
    ControlInt(Span<'a>),
    Make(Span<'a>),
    VariableDeclaration(Span<'a>),
    AddAssign(Span<'a>),
    Condition(Span<'a>),
    Loop(Span<'a>),
    Macro(Span<'a>),
    MacroName(Span<'a>),
    ParameterList(Span<'a>),
    Segment(Span<'a>),
    MacroSegment(Span<'a>),
    Invocation(Span<'a>),
    Expression(Span<'a>),
    Operator(Span<'a>),
    Value(Span<'a>),
    Variable(Span<'a>),
    Queries(Span<'a>),
    Eoi(Span<'a>),
}

impl<'a> AstNode<'a> {
    pub(crate) fn new(node_type: NodeType<'a>) -> Self {
        AstNode {
            node_type,
            children: Vec::new(),
        }
    }
    //Insert child nodes
    pub(crate) fn add_child(&mut self, child: AstNode<'a>) {
        self.children.push(child);
    }
}

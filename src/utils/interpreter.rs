use crate::utils::ast_definition::{Ast, AstNode, NodeType};
use crate::utils::turtle::Turtle;
use pest::Span;
use std::collections::{HashMap, VecDeque};

// return value of the parsing function
pub enum Res {
    Float(f32),
    Bool(bool),
    Success,
}

impl Res {
    fn as_f32(&self) -> f32 {
        match self {
            Res::Float(f) => *f,
            _ => panic!("Value is not a float"),
        }
    }

    fn as_bool(&self) -> bool {
        match self {
            Res::Bool(f) => *f,
            _ => panic!("Value is not a bool"),
        }
    }

    fn is_f32(&self) -> bool {
        matches!(self, Res::Float(_))
    }

    fn is_bool(&self) -> bool {
        matches!(self, Res::Bool(_))
    }
}

fn f32_eq(a: f32, b: f32) -> bool {
    (a - b).abs() < 1e-7_f32
}

fn f32_lt(a: f32, b: f32) -> bool {
    (b - a) > 1e-7_f32
}

fn f32_gt(a: f32, b: f32) -> bool {
    (a - b) > 1e-7_f32
}

fn f32_to_i32(input: f32) -> Result<i32, String> {
    let nearest = input.round();
    if (input - nearest).abs() < 1e-7 {
        if nearest >= i32::MIN as f32 && nearest <= i32::MAX as f32 {
            Ok(nearest as i32)
        } else {
            Err("Wrong Type".to_string())
        }
    } else {
        Err("Wrong Type".to_string())
    }
}

// Stores the parameter list of the macro and the code block expected to be executed when the macro is called.
struct MacroDefinition<'a> {
    argument_list: Vec<String>,
    block: &'a AstNode<'a>,
}

impl<'a> MacroDefinition<'a> {
    fn new(argument_list: Vec<String>, block: &'a AstNode<'a>) -> Self {
        MacroDefinition {
            argument_list,
            block,
        }
    }
}

// Stack Frame
struct Scope {
    variables: HashMap<String, Res>, // Storing Local Variables
}

impl Scope {
    fn new() -> Self {
        Scope {
            variables: HashMap::new(),
        }
    }
}

// Interpreter runtime environment
// Global variables, macro declarations, call stacks and state of the Turtle
pub struct Interpreter<'a> {
    global_variable: HashMap<String, Res>,
    macro_lg: HashMap<String, MacroDefinition<'a>>,
    scopes: VecDeque<Scope>, // Implementing a function(macro) call stack using a double-ended queue
    turtle: Turtle<'a>,
}

impl<'a> Interpreter<'a> {
    pub fn new(turtle: Turtle<'a>) -> Self {
        Interpreter {
            global_variable: HashMap::new(),
            macro_lg: HashMap::new(),
            scopes: VecDeque::new(),
            turtle,
        }
    }
    // Recursive Descent Parsing
    pub fn execute(&mut self, ast: &'a Ast<'a>) -> Result<(), String> {
        let root = &ast.root;
        for child in &root.children {
            self.visit(child)?;
        }
        Ok(())
    }

    // Visits a node and calls the corresponding parsing function according to the grammar rule to which the node's type belongs.
    fn visit(&mut self, node: &'a AstNode<'a>) -> Result<Res, String> {
        match node.node_type {
            NodeType::Expression(_span) => self.handle_expression(node),
            NodeType::ControlFloat(span) => self.handle_control_float(node, &span),
            NodeType::ControlInt(span) => self.handle_control_int(node, &span),
            NodeType::ControlUnary(span) => self.handle_control_unary(&span),
            NodeType::Make(_span) => self.handle_make(node),
            NodeType::AddAssign(_span) => self.handle_add_assign(node),
            NodeType::Segment(_span) => self.handle_segment(node),
            NodeType::Condition(_span) => self.handle_condition(node),
            NodeType::Loop(_span) => self.handle_loop(node),
            NodeType::Macro(_span) => self.handle_macro_definition(node),
            NodeType::Invocation(_span) => self.handle_invocation(node),
            NodeType::MacroSegment(_span) => self.handle_macro_segment(node),
            NodeType::Eoi(_span) => Ok(Res::Success),
            _ => Err("Error".to_string()),
        }
    }

    fn handle_expression(&mut self, node: &'a AstNode<'a>) -> Result<Res, String> {
        match node.children[0].node_type {
            NodeType::Operator(span) => self.handle_operator(node, &span),
            NodeType::Value(span) => self.handle_value(&span),
            NodeType::Variable(span) => self.handle_variable(&span),
            NodeType::Queries(span) => self.handle_queries(&span),
            _ => Err("Expression Error".to_string()),
        }
    }

    fn handle_control_float(&mut self, node: &'a AstNode<'a>, span: &Span) -> Result<Res, String> {
        let value = self.visit(&node.children[0])?;
        let value_f32 = value.as_f32();

        match span.as_str().split_whitespace().next() {
            Some("FORWARD") => self.turtle.action(value_f32, 0),
            Some("RIGHT") => self.turtle.action(value_f32, 90),
            Some("BACK") => self.turtle.action(value_f32, 180),
            Some("LEFT") => self.turtle.action(value_f32, 270),
            Some("SETX") => self
                .turtle
                .set_values(None, Some(value_f32), None, None, None),
            Some("SETY") => self
                .turtle
                .set_values(None, None, Some(value_f32), None, None),
            _ => return Err("Wrong Command".to_string()),
        }
        Ok(Res::Success)
    }

    fn handle_control_int(&mut self, node: &'a AstNode<'a>, span: &Span) -> Result<Res, String> {
        let value = self.visit(&node.children[0])?;
        let value_i32 = f32_to_i32(value.as_f32())?;

        match span.as_str().split_whitespace().next() {
            Some("TURN") => self.turtle.set_values(
                None,
                None,
                None,
                Some(self.turtle.heading + value_i32),
                None,
            ),
            Some("SETHEADING") => self
                .turtle
                .set_values(None, None, None, Some(value_i32), None),
            Some("SETPENCOLOR") => {
                self.turtle
                    .set_values(None, None, None, None, Some(value_i32 as usize))
            }
            _ => return Err("Wrong Command".to_string()),
        }
        Ok(Res::Success)
    }

    fn handle_control_unary(&mut self, span: &Span) -> Result<Res, String> {
        match span.as_str().split_whitespace().next() {
            Some("PENDOWN") => self.turtle.set_values(Some(true), None, None, None, None),
            Some("PENUP") => self.turtle.set_values(Some(false), None, None, None, None),
            _ => return Err("Wrong Command".to_string()),
        }
        Ok(Res::Success)
    }

    fn handle_operator(&mut self, node: &'a AstNode<'a>, span: &Span) -> Result<Res, String> {
        let left = self.visit(&node.children[1])?;
        let right = self.visit(&node.children[2])?;
        match span.as_str() {
            "+" => Ok(Res::Float(left.as_f32() + right.as_f32())),
            "-" => Ok(Res::Float(left.as_f32() - right.as_f32())),
            "*" => Ok(Res::Float(left.as_f32() * right.as_f32())),
            "/" => Ok(Res::Float(left.as_f32() / right.as_f32())),
            "AND" => Ok(Res::Bool(left.as_bool() && right.as_bool())),
            "OR" => Ok(Res::Bool(left.as_bool() || right.as_bool())),
            "LT" => Ok(Res::Bool(f32_lt(left.as_f32(), right.as_f32()))),
            "GT" => Ok(Res::Bool(f32_gt(left.as_f32(), right.as_f32()))),
            "EQ" => {
                if left.is_f32() && right.is_f32() {
                    Ok(Res::Bool(f32_eq(left.as_f32(), right.as_f32())))
                } else if left.is_bool() && right.is_bool() {
                    Ok(Res::Bool(left.as_bool() == right.as_bool()))
                } else {
                    Err("EQ type mismatch".to_string())
                }
            }
            "NE" => {
                if left.is_f32() && right.is_f32() {
                    Ok(Res::Bool(!f32_eq(left.as_f32(), right.as_f32())))
                } else if left.is_bool() && right.is_bool() {
                    Ok(Res::Bool(left.as_bool() != right.as_bool()))
                } else {
                    Err("NE type mismatch".to_string())
                }
            }
            _ => Err("Unsupported operator".to_string()),
        }
    }

    fn handle_value(&mut self, span: &Span) -> Result<Res, String> {
        let raw_str = span.as_str().trim_matches('\"');

        raw_str
            .parse::<f32>()
            .map(Res::Float)
            .or_else(|_| match raw_str.to_uppercase().as_str() {
                "TRUE" => Ok(Res::Bool(true)),
                "FALSE" => Ok(Res::Bool(false)),
                _ => Err("Wrong Type".to_string()),
            })
    }

    // First, search for the variable in the local variables which stored in the call stack
    // If not found, then access the global variable area of the Interpreter structure
    fn handle_variable(&mut self, span: &Span) -> Result<Res, String> {
        let variable_name = span.as_str().trim_matches(':');
        for scope in self.scopes.iter().rev() {
            match scope.variables.get(variable_name) {
                Some(Res::Float(float)) => return Ok(Res::Float(*float)),
                Some(Res::Bool(boolean)) => return Ok(Res::Bool(*boolean)),
                _ => (),
            }
        }
        match self.global_variable.get(variable_name) {
            Some(Res::Float(float)) => Ok(Res::Float(*float)),
            Some(Res::Bool(boolean)) => Ok(Res::Bool(*boolean)),
            _ => Err("Unknown Variable".to_string()),
        }
    }

    fn handle_queries(&mut self, span: &Span) -> Result<Res, String> {
        match span.as_str() {
            "XCOR" => Ok(Res::Float(self.turtle.xcor)),
            "YCOR" => Ok(Res::Float(self.turtle.ycor)),
            "HEADING" => Ok(Res::Float(self.turtle.heading as f32)),
            "COLOR" => Ok(Res::Float(self.turtle.color as f32)),
            _ => Err("Queries Error".to_string()),
        }
    }

    // Initialization of global variables
    fn handle_make(&mut self, node: &'a AstNode<'a>) -> Result<Res, String> {
        match node.children[0].node_type {
            NodeType::VariableDeclaration(span) => {
                let variable_name = span.as_str().trim_matches('\"');
                let value = self.visit(&node.children[1])?;
                self.global_variable
                    .insert(variable_name.to_string(), value);
                Ok(Res::Success)
            }
            _ => Err("Make Error".to_string()),
        }
    }

    fn handle_add_assign(&mut self, node: &'a AstNode<'a>) -> Result<Res, String> {
        match node.children[0].node_type {
            NodeType::VariableDeclaration(span) => {
                let variable_name = span.as_str().trim_matches('\"');
                let value = self.visit(&node.children[1])?;
                let value = Res::Float(
                    self.global_variable.get(variable_name).unwrap().as_f32() + value.as_f32(),
                );
                self.global_variable
                    .insert(variable_name.to_string(), value);
                Ok(Res::Success)
            }
            _ => Err("AddAssign Error".to_string()),
        }
    }

    // If condition is true then the child node is accessed to execute the corresponding block of code
    fn handle_condition(&mut self, node: &'a AstNode<'a>) -> Result<Res, String> {
        if self.visit(&node.children[0])?.as_bool() {
            self.visit(&node.children[1])?;
        }
        Ok(Res::Success)
    }

    // When condition is true, accessing the child node executes the corresponding block of code until condition is false
    fn handle_loop(&mut self, node: &'a AstNode<'a>) -> Result<Res, String> {
        while self.visit(&node.children[0])?.as_bool() {
            self.visit(&node.children[1])?;
        }
        Ok(Res::Success)
    }

    // Definition of Macros
    // Store the name of the macro, the parameter list of the macro
    // and the root node of the code block to be executed in the Interpreter structure.
    fn handle_macro_definition(&mut self, node: &'a AstNode<'a>) -> Result<Res, String> {
        if let NodeType::MacroName(span) = node.children[0].node_type {
            let macro_name = span.as_str().to_string();
            if let NodeType::ParameterList(_) = node.children[1].node_type {
                let para_list_result: Result<Vec<String>, String> = node.children[1]
                    .children
                    .iter()
                    .map(|child| {
                        if let NodeType::VariableDeclaration(var_span) = child.node_type {
                            Ok(var_span.as_str().trim_matches('\"').to_string())
                        } else {
                            Err("Macro Definition Error".to_string())
                        }
                    })
                    .collect();
                let para_list = para_list_result?;
                if let NodeType::MacroSegment(_) = node.children[2].node_type {
                    self.macro_lg.insert(
                        macro_name,
                        MacroDefinition::new(para_list, &node.children[2]),
                    );
                    return Ok(Res::Success);
                }
            }
        }
        Err("Macro Definition Error".to_string())
    }

    // Invoke the macro:
    //      Find the definition of the macro being called in the Interpreter structure.
    //      Initialize a Scope to store local variables.
    //      Push the Scope into the call stack (VecDeque).
    //      Execute the macro.
    //      Pop the Scope out of the stack.
    fn handle_invocation(&mut self, node: &'a AstNode<'a>) -> Result<Res, String> {
        let mut local_scope = Scope::new();
        if let NodeType::MacroName(span) = node.children[0].node_type {
            let (argument_list, block) =
                if let Some(macro_definition) = self.macro_lg.get(span.as_str()) {
                    (
                        macro_definition.argument_list.clone(),
                        macro_definition.block,
                    )
                } else {
                    return Err("Unknown Call Error".to_string());
                };
            for (index, arg_name) in argument_list.iter().enumerate() {
                local_scope
                    .variables
                    .insert(arg_name.to_string(), self.visit(&node.children[index + 1])?);
            }
            self.scopes.push_back(local_scope);
            if self.visit(block).is_err() {
                self.scopes.pop_back();
                return Err("Macro Invocation Error".to_string());
            }
            self.scopes.pop_back();
            return Ok(Res::Success);
        }
        Err("Macro Invocation Error".to_string())
    }

    //Execute the code block.
    fn handle_segment(&mut self, node: &'a AstNode<'a>) -> Result<Res, String> {
        for child in &node.children {
            self.visit(child)?;
        }
        Ok(Res::Success)
    }

    //Execute the code block.
    fn handle_macro_segment(&mut self, node: &'a AstNode<'a>) -> Result<Res, String> {
        for child in &node.children {
            self.visit(child)?;
        }
        Ok(Res::Success)
    }
}

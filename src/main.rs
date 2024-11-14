use std::fmt::Display;

use scopegraphs::{completeness::ImplicitClose, render::RenderSettings, Scope, ScopeGraph, Storage};

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
enum Label {
    Parent,
    Declaration,
    Reference,
}


// unused?
#[derive(PartialEq, Eq, Hash)]
enum Relation {
    Declaration,
}

#[derive(Hash, Default, Clone)]
enum Data {
    #[default]
    NoData,
    Variable(String, Type),
}

#[derive(PartialEq, Eq, Hash, Clone)]
enum Type {
    Num,
    Decl(Box<Type>, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Num => write!(f, "num"),
            Type::Decl(param_type, return_type) => write!(f, "({} -> {})", param_type, return_type),
        }
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Parent => write!(f, "Parent"),
            Label::Declaration => write!(f, "Declaration"),
            Label::Reference => write!(f, "Reference"),
        }
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::NoData => write!(f, "NoData"),
            Data::Variable(name, ty) => write!(f, "{}: {}", name, ty),
        }
    }
}

const ALL_LABELS: [Label; 3] = [Label::Parent, Label::Declaration, Label::Reference];
impl scopegraphs::Label for Label {
    fn iter() -> impl Iterator<Item = Self>
    where
        Self: Sized {
        ALL_LABELS.into_iter()
    }

    fn iter_ref() -> impl Iterator<Item = &'static Self>
    where
        Self: Sized + 'static {
        ALL_LABELS.iter()
    }
}

impl scopegraphs::render::RenderScopeLabel for Label {
    fn render(&self) -> String {
        self.to_string()
    }
}

impl scopegraphs::render::RenderScopeData for Data {
    fn render_node(&self) -> Option<String> {
        Some(self.to_string())
    }

    fn render_node_label(&self) -> Option<String> {
        None
    }

    fn extra_edges(&self) -> Vec<scopegraphs::render::EdgeTo> {
        Vec::new()
    }

    fn definition(&self) -> bool {
        self.render_node().is_some()
    }
}


enum Expression {
    Literal(i32),
    Var(String),
    // arg1, arg2
    Add(Box<Expression>, Box<Expression>),
    // parameter, parameter type, body
    Func(String, Type, Box<Expression>),
    Call(Box<Expression>, Box<Expression>),
    // Name, value, value_type tail
    Decl(String, Box<Expression>, Box<Expression>),
}

impl Expression {
    /// returns an example program, equivalent to the one in the paper
    fn example_progam() -> Self {
        Expression::Decl(
        "x".to_string(), Box::new(Expression::Literal(3)), // let x = 3 in
        Box::new(Expression::Decl("f".to_string(), // let f = fun(x: num) { x } in
            Box::new(Expression::Func("x".to_string(), Type::Num, Box::new(Expression::Var("x".to_string())))),
            Box::new(Expression::Call(Box::new(Expression::Var("f".to_string())), Box::new(Expression::Var("x".to_string()))))
        )))
    }

    fn expr_type(&self) -> Type {
        match self {
            Expression::Literal(_) => Type::Num,
            Expression::Var(_) => {
                Type::Num
                // todo!("query sg for type")
            },
            Expression::Add(expression, expression1) => Type::Num,
            Expression::Func(_, param_type, expression) => Type::Decl(Box::new(param_type.clone()), Box::new(expression.expr_type())),
            Expression::Call(func, _) => todo!("query sg for type of func"),
            Expression::Decl(_, body, _) => body.expr_type(),
        }
    }

    fn construct_scopes(&self, sg: &mut StlcGraph<'_>, prev_scope: &mut Scope) {
        println!("prev_scope: {0:?}", prev_scope);
        match self {
            Expression::Literal(_) => (), // dont create scope for a number
            Expression::Var(name) => {
                sg.add_decl(*prev_scope, Label::Reference, Data::Variable(name.clone(), self.expr_type())).unwrap();
            },
            Expression::Add(expression, expression1) => todo!(),
            Expression::Call(func, param) => {
                match (&**func, &**param) {
                    (Self::Var(func_name), Self::Var(param_name)) => {
                        sg.add_decl(*prev_scope, Label::Reference, Data::Variable(func_name.to_string(), func.expr_type())).unwrap();
                        sg.add_decl(*prev_scope, Label::Reference, Data::Variable(param_name.to_string(), param.expr_type())).unwrap();
                    }
                    _ => panic!("Only variables are supported for now")
                }
            },
            Expression::Func(param_name, param_type, body) => {
                // add new scope for the function
                let mut new_scope = sg.add_scope_default();
                sg.add_edge(*prev_scope, Label::Parent, new_scope).unwrap();

                // add scope for parameter declaration
                let param_data = Data::Variable(param_name.to_string(), param_type.clone());
                sg.add_decl(new_scope, Label::Declaration, param_data).unwrap();

                // construct scopes for body using new scope
                body.construct_scopes(sg, &mut new_scope);
            },
            Expression::Decl(name, body, tail) => {
                // add new scope for the current "line"
                let mut new_scope = sg.add_scope_default();
                sg.add_edge(*prev_scope, Label::Parent, new_scope).unwrap();

                // add scope for var declaration
                let data = Data::Variable(name.to_string(), body.expr_type());
                sg.add_decl(new_scope, Label::Declaration, data).unwrap();

                // construct scopes for body and tail using new_scope
                body.construct_scopes(sg, prev_scope);
                tail.construct_scopes(sg, &mut new_scope);
            },
        }
    }
}


type StlcGraph<'s> = ScopeGraph<'s, Label, Data, ImplicitClose<Label>>;


fn main() {
    let storage = Storage::new();
    let mut sg = StlcGraph::new(&storage, ImplicitClose::default());
    let mut s0 = sg.add_scope_default();
    Expression::example_progam().construct_scopes(&mut sg, &mut s0);

    sg.render_to("output.mmd", RenderSettings::default()).unwrap();
}

use std::fmt::Display;
use std::sync::atomic::{AtomicUsize, Ordering};

use scopegraphs::query_regex;
use scopegraphs::{completeness::ImplicitClose, label_order, render::RenderSettings, Scope, ScopeGraph, Storage};
use scopegraphs::resolve::Resolve;


static mut SCOPE_NUM: AtomicUsize = AtomicUsize::new(0);

#[derive(PartialEq, Eq, Hash, Debug, Clone, scopegraphs::Label, Copy)]
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

#[derive(Hash, Default, Clone, Debug, PartialEq, Eq)]
enum Data {
    #[default]
    NoData,
    ScopeNum(usize),
    Variable(String, Type),
}

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
enum Type {
    Num,
    Fun(Box<Type>, Box<Type>),
}

impl Type {
    fn fun(param_type: Type, return_type: Type) -> Type {
        Type::Fun(Box::new(param_type), Box::new(return_type))
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Num => write!(f, "num"),
            Type::Fun(param_type, return_type) => write!(f, "({} -> {})", param_type, return_type),
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
            Data::ScopeNum(num) => write!(f, "{}", num),
        }
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

#[derive(Debug)]
enum Expression {
    Literal(i32),
    Var(String),
    // arg1, arg2
    Add(Box<Expression>, Box<Expression>),
    // parameter, parameter type, body
    Func(String, Type, Box<Expression>),
    Call(Box<Expression>, Box<Expression>),
    // Name, value, value_type tail
    Let(String, Box<Expression>, Box<Expression>),
}

impl Expression {

    fn var(name: impl ToString) -> Self {
        Expression::Var(name.to_string())
    }
    // just some utility
    fn add(lhs: Expression, rhs: Expression) -> Self {
        Expression::Add(Box::new(lhs), Box::new(rhs))
    }

    fn func(param_name: impl ToString, param_type: Type, body: Expression) -> Self {
        Expression::Func(param_name.to_string(), param_type, Box::new(body))
    }

    fn call(func: Expression, param: Expression) -> Self {
        Expression::Call(Box::new(func), Box::new(param))
    }

    fn let_expr(name: impl ToString, body: Expression, tail: Expression) -> Self {
        Expression::Let(name.to_string(), Box::new(body), Box::new(tail))
    }

    /*
        let x = 3 in
        let y = x in
        let f = fun(x: num) { let z = x in z } in
        f y
     */
    /// returns an example program, equivalent to the one in the paper
    fn example_progam() -> Self {
        Expression::let_expr("x", Expression::Literal(3),
        Expression::let_expr("y", Expression::Var("x".to_string()),
        Expression::let_expr("f", Expression::func("x", Type::Num,
            Expression::let_expr("z", Expression::var("x"), Expression::var("z"))),
        Expression::call(Expression::var("f"), Expression::var("y"))
        )))
    }


    // making this unsafe since i really quickly want a global counter, ill make it nice later i promise
    unsafe fn expr_type(&self, sg: &StlcGraph<'_>, prev_scope: Scope) -> Type {
        // I think we never wanna change the label order and wellformedness, so lets define that here
        let base_query = sg.query()
        .with_path_wellformedness(query_regex!(Label: Parent*Declaration))
        .with_label_order(label_order!(Label: Parent < Declaration));
        match self {
            Expression::Literal(_) => Type::Num,
            Expression::Var(name) => {
                // query the scopegraph for the name of this thing and return the type
                let var_query = base_query
                .with_data_wellformedness(|data: &Data| -> bool {
                    matches!(data, Data::Variable(d_name, _) if d_name == name)
                })
                .resolve(prev_scope);

                match var_query.into_iter().nth(0).expect("Variable not found").data() {
                    Data::Variable(_, ty) => ty.clone(),
                    _ => panic!("Variable found but no type")
                }
            },
            Expression::Add(lhs, rhs) => {
                // check if both expressions are numbers and then return a number
                let ty1 = lhs.expr_type(sg, prev_scope);
                let ty2 = rhs.expr_type(sg, prev_scope);
                if ty1 == Type::Num && ty2 == Type::Num {
                    Type::Num
                } else {
                    panic!("Addition of non-numbers")
                }
            },
            Expression::Func(param_name, param_type, body) => {
                // add new scope for the function
                let new_scope = sg.add_scope(Data::ScopeNum(SCOPE_NUM.fetch_add(1, Ordering::Relaxed)));
                sg.add_edge(new_scope, Label::Parent, prev_scope).unwrap();

                // add scope for parameter declaration
                let param_data = Data::Variable(param_name.to_string(), param_type.clone());
                sg.add_decl(new_scope, Label::Declaration, param_data).unwrap();

                // construct scopes for body using new scope
                let body_type = body.expr_type(sg, new_scope);
                Type::fun(param_type.clone(), body_type)
            },
            Expression::Call(func, param) => {
                let func_type = func.expr_type(sg, prev_scope);

                let (t1, t2) = match func_type {
                    Type::Fun(t1, t2) => (t1, t2),
                    _ => panic!("Attempted to call non-function")
                };

                let param_type = param.expr_type(sg, prev_scope);
                if t1.as_ref() != &param_type {
                    panic!("Parameter type mismatch")
                }
                *t2
            },
            Expression::Let(name, body, tail) => {
                // add new scope for the current "line"
                let new_scope = sg.add_scope(Data::ScopeNum(SCOPE_NUM.fetch_add(1, Ordering::Relaxed)));
                // let new_scope = sg.add_scope_default();
                sg.add_edge(new_scope, Label::Parent, prev_scope).unwrap();

                // add scope for var declaration
                let data = Data::Variable(name.to_string(), body.expr_type(sg, prev_scope));
                sg.add_decl(new_scope, Label::Declaration, data).unwrap();

                // construct scopes for body and tail using new_scope
                tail.expr_type(sg, new_scope)
            },
        }
    }
}


type StlcGraph<'s> = ScopeGraph<'s, Label, Data, ImplicitClose<Label>>;


fn main() {
    let storage = Storage::new();
    let sg = StlcGraph::new(&storage, ImplicitClose::default());
    // let s0 = sg.add_scope_default();
    unsafe {
        let s0 = sg.add_scope(Data::ScopeNum(SCOPE_NUM.fetch_add(1, Ordering::Relaxed)));
        Expression::example_progam().expr_type(&sg, s0)
    };

    sg.render_to("output.mmd", RenderSettings::default()).unwrap();
}

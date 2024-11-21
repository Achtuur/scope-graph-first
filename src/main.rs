use std::fmt::Display;
use std::sync::atomic::{AtomicUsize, Ordering};

use scopegraphs::query_regex;
use scopegraphs::{completeness::ImplicitClose, label_order, render::RenderSettings, Scope, ScopeGraph, Storage};
use scopegraphs::resolve::Resolve;


static SCOPE_NUM: AtomicUsize = AtomicUsize::new(0);

#[derive(PartialEq, Eq, Hash, Debug, Clone, scopegraphs::Label, Copy)]
enum Label {
    Parent,
    Declaration,
    Record,
    Extension,
}


#[derive(Hash, Default, Clone, Debug, PartialEq, Eq)]
enum Data {
    #[default]
    NoData,
    ScopeNum(usize),
    RecScope(usize),
    Variable(String, Type),
}

impl Data {
    pub fn datatype(&self) -> Option<&Type> {
        match self {
            Data::Variable(_, ty) => Some(ty),
            _ => None
        }
    }
}

#[derive(Hash, Clone, Debug, PartialEq, Eq)]
enum Type {
    Num,
    Bool,
    Fun(Box<Type>, Box<Type>),
    // number is the scope number
    Record(usize),
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
            Type::Bool => write!(f, "bool"),
            Type::Fun(param_type, return_type) => write!(f, "({param_type} -> {return_type})"),
            Type::Record(n) => write!(f, "REC({n})")
        }
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Parent => write!(f, "Parent"),
            Label::Declaration => write!(f, "Declaration"),
            Label::Extension => write!(f, "Extension"),
            Label::Record => write!(f, "Record"),
        }
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::NoData => write!(f, "NoData"),
            Data::Variable(name, ty) => write!(f, "{name}: {ty}"),
            Data::ScopeNum(num) => write!(f, "{num}"),
            Data::RecScope(num) => write!(f, "Rec: {num}"),
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

/// Expressions that can appear in the SLTC language
///
/// Extended with records
#[derive(Debug)]
enum Expression {
    Literal(i32),
    Boolean(bool),
    Var(String),
    // arg1, arg2
    Add(Box<Expression>, Box<Expression>),
    // parameter, parameter type, body
    Func {
        param_name: String,
        param_type: Type,
        body: Box<Expression>,
    },
    Call {
        func: Box<Expression>,
        param: Box<Expression>,
    },
    Let {
        name: String,
        body: Box<Expression>,
        tail: Box<Expression>,
    },
    Record(Vec<(String, Expression)>),

    RecordAccess {
        record: Box<Expression>,
        field: String,
    },

    Extension {
        /// Added/overwritten fields
        extension: Box<Expression>,
        /// Parent record
        parent: Box<Expression>,
    },

    With {
        record: Box<Expression>,
        body: Box<Expression>,
    },
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
        Expression::Func{
            param_name: param_name.to_string(),
            param_type,
            body: Box::new(body)
        }
    }

    fn call(func: Expression, param: Expression) -> Self {
        Expression::Call{
            func: Box::new(func),
            param: Box::new(param)
        }
    }

    fn let_expr(name: impl ToString, body: Expression, tail: Expression) -> Self {
        Expression::Let{
            name: name.to_string(),
            body: Box::new(body),
            tail: Box::new(tail)
        }
    }

    fn rec_access(record: impl ToString, field: impl ToString) -> Self {
        Expression::RecordAccess{
            record: Box::new(Expression::Var(record.to_string())),
            field: field.to_string()
        }
    }

    /*
        let x = false in
        let x = 3 in
        let y = x in
        let f = fun(x: num) { let z = x in true } in
        f y
     */
    /// returns an example program, equivalent to the one in the paper
    fn example_program() -> Self {
        Expression::let_expr("x", Expression::Boolean(false),
        Expression::let_expr("x", Expression::Literal(3),
        Expression::let_expr("y", Expression::Var("x".to_string()),
        Expression::let_expr("f", Expression::func("x", Type::Num,
            Expression::let_expr("z", Expression::var("x"), Expression::Boolean(false))), // x and z are unused
        Expression::call(Expression::var("f"), Expression::var("y"))
        ))))
    }

    fn example_program_rec() -> Self {
        let rec_expr = Expression::Record(vec![(String::from("x"), Expression::Literal(3)), (String::from("y"), Expression::Boolean(false))]);
        let ext_rec = Expression::Record(vec![(String::from("z"), Expression::Literal(4)), (String::from("x"), Expression::Boolean(true))]);
        Expression::let_expr("r", rec_expr,
        Expression::let_expr("a", Expression::rec_access("r", "x"),
        Expression::let_expr("q", Expression::Extension {
            extension: Box::new(ext_rec),
            parent: Box::new(Expression::var("r")),
        },
        Expression::let_expr("b", Expression::rec_access("q", "x"),
        Expression::var("a")
        ))))
    }


    // making this unsafe since i really quickly want a global counter, ill make it nice later i promise
    fn expr_type(&self, sg: &StlcGraph<'_>, prev_scope: Scope) -> Type {
        match self {
            Expression::Literal(_) => Type::Num,
            Expression::Boolean(_) => Type::Bool,
            Expression::Var(name) => {
                // query the scopegraph for the name of this thing and return the type
                let var_query = sg.query()
                .with_path_wellformedness(query_regex!(Label: Parent*Declaration))
                .with_label_order(label_order!(Label: Declaration < Parent))
                .with_data_wellformedness(|data: &Data| -> bool {
                    matches!(data, Data::Variable(d_name, _) if d_name == name)
                })
                // .with_data_equivalence(|data1: &Data, data2: &Data| -> bool {
                //     data1 == data2 // name and type should be the same
                // })
                .resolve(prev_scope);
                // println!("var_query: {:#?}", var_query);
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
            Expression::Func{param_name, param_type, body} => {
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
            Expression::Call{func, param} => {
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
            Expression::Let{name, body, tail} => {
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
            Expression::Record(fields) => {
                // create a scope n that declares the record parameters
                let record_scope = sg.add_scope(Data::RecScope(SCOPE_NUM.fetch_add(1, Ordering::Relaxed)));

                // declare fields
                for (name, expr) in fields {
                    let field_type = expr.expr_type(sg, prev_scope);
                    let field_data = Data::Variable(name.to_string(), field_type);
                    sg.add_decl(record_scope, Label::Declaration, field_data).unwrap();
                }

                Type::Record(record_scope.0)
            },
            Expression::RecordAccess { record, field } => {
                let record_type = record.expr_type(sg, prev_scope);
                let Type::Record(scope_num) = record_type else {
                    panic!("RecordAccess on non-record")
                };

                // query scope_num for field
                let query = sg.query()
                .with_path_wellformedness(query_regex!(Label: (Record|Extension)*Declaration)) // follow R or E edge until declaration
                .with_label_order(label_order!(Label: Record < Extension, Declaration < Record, Declaration < Extension)) // R < E, $ < R, $ < E
                .with_data_wellformedness(|data: &Data| -> bool {
                    matches!(data, Data::Variable(d_name, _) if d_name == field)
                })
                .resolve(Scope(scope_num));
                query
                    .get_only_item().expect("Field not found")
                    .data()
                    .datatype()
                    .expect("Data has no type")
                    .clone()
            },
            Expression::Extension { extension, parent: original } => {
                let ext_scope = sg.add_scope(Data::ScopeNum(SCOPE_NUM.fetch_add(1, Ordering::Relaxed)));
                // extension must be record type
                let ext_t = extension.expr_type(sg, prev_scope);
                let Type::Record(ext_rec) = ext_t else {
                    panic!("Extension type is not record")
                };

                // original must be record type
                let orig_t = original.expr_type(sg, prev_scope);
                let Type::Record(r) = orig_t else {
                    panic!("Extending a non-record type")
                };

                // ext_scope -R> ext_rec
                // ext_scope -E> r
                sg.add_edge(ext_scope, Label::Record, Scope(ext_rec)).unwrap();
                sg.add_edge(ext_scope, Label::Extension, Scope(r)).unwrap();
                Type::Record(ext_scope.0)
            },
            Expression::With { record, body } => todo!(),
        }
    }
}


type StlcGraph<'s> = ScopeGraph<'s, Label, Data, ImplicitClose<Label>>;


fn main() {
    let storage = Storage::new();
    let sg = StlcGraph::new(&storage, ImplicitClose::default());
    let s0 = sg.add_scope(Data::ScopeNum(SCOPE_NUM.fetch_add(1, Ordering::Relaxed)));
    Expression::example_program_rec().expr_type(&sg, s0);

    sg.render_to("output.mmd", RenderSettings::default()).unwrap();
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_equivalence() {
        let t1 = Data::Variable("x".to_string(), Type::Num);
        let t2 = Data::Variable("x".to_string(), Type::Bool);
        assert_ne!(t1, t2);

        let t3 = Data::Variable("x".to_string(), Type::Num);
        assert_eq!(t1, t3);

        let t4 = Data::Variable("y".to_string(), Type::Num);
        assert_ne!(t1, t4);
    }
}
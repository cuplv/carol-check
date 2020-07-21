extern crate z3;

use std::rc::Rc;

use z3::{DatatypeBuilder, SatResult, Solver, Sort};
use z3::ast::{Ast, Datatype, Int};

trait CARD {
    type State;
    type Effect;
    type Conref;
}

enum Val<A> {
    Var(String),
    Const(A),
}

enum Comp<D: CARD, A> {
    Ret(Val<A>),
    Query(String, D::Conref, Rc<Comp<D,A>>),
    Issue(D::Effect, Rc<Comp<D,A>>),
}

impl<D: CARD,A> Comp<D,A> {
    fn ret(a: Val<A>) -> Comp<D,A> {
        Comp::Ret(a)
    }
    fn query(x: String, c: D::Conref, m: Comp<D,A>) -> Comp<D,A> {
        Comp::Query(x, c, Rc::new(m))
    }
    fn issue(e: D::Effect, m: Comp<D,A>) -> Comp<D,A> {
        Comp::Issue(e, Rc::new(m))
    }
}
    

fn main() {
    let cfg = z3::Config::new();
    let ctx = z3::Context::new(&cfg);

    let solver = Solver::new(&ctx);

    // Like Rust's Option<int> type
    let option_int = DatatypeBuilder::new(&ctx)
            .variant("None", &[])
            .variant("Some", &[("value", &Sort::int(&ctx))])
            .finish("OptionInt");
    
    // Assert x.is_none()
    let x = Datatype::new_const(&ctx, "x", &option_int.sort);
    solver.assert(&option_int.variants[0].tester.apply(&[&x.into()]).as_bool().unwrap());
    
    // Assert y == Some(3)
    let y = Datatype::new_const(&ctx, "y", &option_int.sort);
    let value = option_int.variants[1].constructor.apply(&[&Int::from_i64(&ctx, 3).into()]);
    solver.assert(&y._eq(&value.as_datatype().unwrap()));
    
    assert_eq!(solver.check(), SatResult::Sat);
    let model = solver.get_model();
    
    // Get the value out of Some(3)
    let ast = option_int.variants[1].accessors[0].apply(&[&y.into()]);
    assert_eq!(3, model.eval(&ast.as_int().unwrap()).unwrap().as_i64().unwrap());

    println!("Hello, world!");
    println!("{:?}", ast);
}

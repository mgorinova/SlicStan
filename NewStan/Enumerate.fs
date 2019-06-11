module Enumerate

open NewStanSyntax

let to_lpf (d:Dist) (x:Ide) =
    // FIXME: only works for discrete distributions; need to extend
    match d with
    | Dist(name, args) -> Prim(name + "_lpmf", Var(x)::args )
    | _ -> failwith "user defined distributions not supported"

let rec enumerate_S (accumulator_type: TypePrim) (accumulator: LValue) (S: S) =
   match S with
    | Block(((Int, Model), x), s) -> 
    (* x is discrete; want to enumerate it
    vector[support(x)] lp;
    for x in support(x) {
        s[target -> lp[x]] // consider only discrete params that go from 1 to N for some N.
    }
    accumulator += log_sum_exp(lp)
    int x = categorical_rng(lp)
    *)
    //FIXME everthing has support 1..3 for simplisity 
    let new_acc_type = Array(accumulator_type, N(3))
    let new_acc = "lp_" + x
    Block(((new_acc_type, Model), new_acc), 
    enumerate_S new_acc_type (I(new_acc)) s
    )

    | Sample(x, d) -> Assign(accumulator, to_lpf d x)
    | Assign(I("target"), e) -> Assign(accumulator, e)

    | Block((t, x), s) -> Block((t, x), enumerate_S accumulator_type accumulator s)
    | Assign(lhs, e) -> Assign(lhs, e)
    | If(e, s1, s2) -> If(e, enumerate_S accumulator_type accumulator s1, enumerate_S accumulator_type accumulator  s2)
    | Seq(s1, s2) -> Seq(enumerate_S accumulator_type accumulator  s1, enumerate_S accumulator_type accumulator  s2)
    | s -> s // skip, data declaration, others 

let enumerate_Prog ((defs, s): NewStanProg): NewStanProg = 
    defs, enumerate_S (Real) (I("target")) s
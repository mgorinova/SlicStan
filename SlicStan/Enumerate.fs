module Enumerate

open SlicStanSyntax
//open Util

let enumerate_Prog (s : S) : S = //((defs, s): NewStanProg): NewStanProg = 

    let assset = Util.assigns_global s
    let hidden_discrete_variables = Set.empty // variables we don't want to re-sample at the end

    let rec enum (accumulator_type: TypePrim) (accumulator: LValue) (indexed_by: Exp list) (S: S) : (S list * S * S list) =
        match S with
        | Decl((T, x), s) -> 
            if Set.contains x assset then 
                let defs, prog, gen = enum accumulator_type accumulator indexed_by s 
                Decl((T, x), Skip)::defs, prog, gen
            else match T with
            | Constrained(Int, upper), Model ->
                (* x is discrete; want to enumerate it
                vector[support(x)] lp;
                for x in support(x) {
                    s[target -> lp[x]] // consider only discrete params that go from 1 to N for some N.
                }
                accumulator += log_sum_exp(lp)
            
                *)
                
                if Set.contains x hidden_discrete_variables then
                    let this_acc_type = Array(Real, upper)
                    let this_acc_name = "lp_" + x
                    let this_acc = A(I(this_acc_name), Var(x))

                    let defs, prog, gen = enum accumulator_type this_acc indexed_by s
                
                    let line2 = Assign(accumulator, Plus(Util.lhs_to_exp accumulator, Prim("log_sum_exp", [Var(this_acc_name)])))
                    let line1 = Seq( For( ((Int, Model), x), N(1), upper, prog ), line2 )
                    let ret_prog = Decl( ((this_acc_type, Model), this_acc_name), line1 )

                    defs, ret_prog, gen
                else
                    let new_acc_type = Array(accumulator_type, upper) 
                    let new_acc_name = "lp_" + x
                    let new_acc_var = Util.indices_list_to_lhs (I(new_acc_name)) indexed_by
                    let new_acc = A(new_acc_var, Var(x)) 
                    
                    let defs, prog, gen = enum new_acc_type new_acc (List.append indexed_by [Var(x)]) s
                    
                    let line1 = Assign(accumulator, Plus(Util.lhs_to_exp accumulator, Prim("log_sum_exp", [Var(new_acc_name)])))
                    let ret_prog = Seq( For( ((Int, Model), x), N(1), upper, prog), line1 )
                    
                    
                    let ret_defs = Decl(((new_acc_type, Model), new_acc_name), Skip) :: defs
                    let ret_gen = Decl( ((Int, GenQuant), x), Assign(I(x), Prim("categorical_rng", [Util.lhs_to_exp new_acc_var])) ) :: gen
                    
                    ret_defs, ret_prog, ret_gen

            | Int, Model -> failwith "Must specify discrete parameter's support using a constrained type int<...>"
            | Array(Constrained(Int, upper), n), Model  -> failwith "not implemented"
            | _ -> 
                let defs, prog, gen = enum accumulator_type accumulator indexed_by s 
                Decl((T, x), Skip) :: defs,  prog,  gen

        | Sample(e, d) -> 
            match accumulator with
            | I("target") -> [], Sample(e, d), []
            | _ -> [], Assign(accumulator, Plus(Util.lhs_to_exp accumulator, (Util.to_lpf d e))), []
        
        | Assign(I("target"), e) -> [], Assign(accumulator, Plus(Util.lhs_to_exp accumulator, e)), []
        
        | Assign(lhs, e) -> [], Assign(lhs, e), []
        | If(e, s1, s2) -> 
            let defs1, prog1, gen1 = enum accumulator_type accumulator indexed_by s1
            let defs2, prog2, gen2 = enum accumulator_type accumulator indexed_by s2 
            List.append defs1 defs2, If(e, prog1, prog2), List.append gen1 gen2
        | Seq(s1, s2) -> 
            let defs1, prog1, gen1 = enum accumulator_type accumulator indexed_by s1
            let defs2, prog2, gen2 = enum accumulator_type accumulator indexed_by s2 
            List.append defs1 defs2, Seq(prog1, prog2), List.append gen1 gen2
        | For(x, l, u, s) -> 
            let defs, prog, gen = enum accumulator_type accumulator indexed_by s 
            defs, For(x, l, u, prog), gen
        | Skip -> [], Skip, []

    let defs, prog, gen = enum (Real) (I("target")) ([]) s

    Seq(Util.SofList defs, Seq(prog, Util.SofList gen))


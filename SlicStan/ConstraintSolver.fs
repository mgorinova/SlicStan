module ConstraintSolver

open Microsoft.Z3
open SlicStanSyntax

type Constraint = Leq of TypeLevel * TypeLevel
type ConstraintInfo = Constraint * string 

let resolve (constraints_info : ConstraintInfo list, level_vars_names : string list) = 

    let constraints = List.map fst constraints_info

    use context = new Context()
    let solver = context.MkOptimize() //.MkSolver() //

    let data_constr = context.MkConstructor("data", "is_data")
    let model_constr = context.MkConstructor("model", "is_model")
    let genquant_constr = context.MkConstructor("genquant", "is_genquant")
    let constructors = [| data_constr; model_constr; genquant_constr |]

    let Level : DatatypeSort = context.MkDatatypeSort("ell", constructors)
    
    let s : Sort [] = [| Level; Level |]
    let levlist : ListSort = context.MkListSort("levlist", Level)
    let leq : FuncDecl = context.MkFuncDecl("leq", s, context.BoolSort) 
    let lub : FuncDecl = context.MkFuncDecl("lub", levlist, Level) 
    let glb : FuncDecl = context.MkFuncDecl("glb", levlist, Level) 

    let (<=.) (x: Expr) (y: Expr) = context.MkApp(leq, [| x; y |]) :?> BoolExpr
    let (=.)  (x: Expr) (y: Expr) = context.MkEq(x, y)
    let nott  (x: BoolExpr)       = context.MkNot(x)
    let lubOf (x: Expr)           = context.MkApp(lub, x)
    let glbOf (x: Expr)           = context.MkApp(glb, x)
    let (&&.) (x: Expr) (y: Expr) = context.MkApp(levlist.ConsDecl, x, y)

    let data = context.MkConst(data_constr.ConstructorDecl)
    let model = context.MkConst(model_constr.ConstructorDecl) 
    let genquant = context.MkConst(genquant_constr.ConstructorDecl)   

    let f (l_name : string) : (string * Expr) = l_name, context.MkConst(l_name, Level)
    let level_vars = List.map f level_vars_names |> Map.ofList


    let rec translate_level (ell : TypeLevel) : Expr =
        match ell with 
        | Data -> data
        | Model -> model
        | Lz -> failwith "Unexpected! Try using resolve_semilattice to resolve type constraints"
        | GenQuant -> genquant
        | LevelVar x -> level_vars.Item(x)
        | Lub ells -> 
            let tr = List.map translate_level ells
                    |> List.fold (fun s el -> context.MkApp(levlist.ConsDecl, el, s)) (context.MkConst(levlist.NilDecl))                   
            context.MkApp(lub, tr)
        | Glb ells -> 
            let tr = List.map translate_level ells
                    |> List.fold (fun s el -> context.MkApp(levlist.ConsDecl, el, s)) (context.MkConst(levlist.NilDecl))                   
            context.MkApp(glb, tr)

    let translate (c : Constraint) : BoolExpr = 
        match c with
        | Leq(l1, l2) ->  (translate_level l1) <=. (translate_level l2)


    let leq_definition = [ data <=. data; data <=. model; data <=. genquant;
                           nott (model <=. data); model <=. model; model <=. genquant;
                           nott (genquant <=. data); nott (genquant <=. model); genquant <=. genquant;]
                        |> List.fold (fun s c -> context.MkAnd(s, c)) (context.MkTrue())
                        |> fun e -> e.Simplify() :?> BoolExpr

    let nil = context.MkConst(levlist.NilDecl)
    let single_data = data &&. nil
    let single_model = model &&. nil
    let single_genquant = genquant &&. nil
    
    let ell_symbol = context.MkSymbol("ell") :> Symbol
    let ell = context.MkConst(ell_symbol, Level)
    let ells_symbol = context.MkSymbol("ells") :> Symbol
    let ells = context.MkConst(ell_symbol, levlist)

    let body = lubOf (ell &&. ells) =. lubOf (ell &&. (lubOf ells &&. nil))

    let q_lub = context.MkForall([| Level :> Sort; levlist :> Sort |], 
                             [| ell_symbol ; ells_symbol |], 
                             body) :> BoolExpr

    let lub_definition = [ lubOf nil =. data; lubOf single_data =. data; 
                           lubOf single_model =. model; lubOf single_genquant =. genquant; 
                           lubOf (data &&. single_data) =. data; lubOf (data &&. single_model) =. model; 
                           lubOf (data &&. single_genquant) =. genquant; lubOf (model &&. single_model) =. model;
                           lubOf (model &&. single_genquant) =. genquant; lubOf (model &&. single_data) =. model;
                           lubOf (genquant &&. single_genquant) =. genquant; lubOf (genquant &&. single_model) =. genquant;
                           lubOf (genquant &&. single_data) =. genquant;

                           // FIXME: the q_lub bit of the theory doesn't seem to work...
                           // Adding the following as a way to solve some of the current 
                           // examples we have, but should fix to be more general.
                           lubOf (data &&. (data &&. single_model)) =. model; lubOf (model &&. (data &&. single_data)) =. model; lubOf (data &&. (model &&. single_data)) =. model;
                           lubOf (model &&. (data &&. single_model)) =. model; lubOf (data &&. (model &&. single_model)) =. model; lubOf (model &&. (model &&. single_data)) =. model;
                           lubOf (data &&. (data &&. single_genquant)) =. genquant; lubOf (genquant &&. (data &&. single_data)) =. genquant; lubOf (data &&. (genquant &&. single_data)) =. genquant;
                           lubOf (genquant &&. (data &&. single_genquant)) =. genquant; lubOf (data &&. (genquant &&. single_genquant)) =. genquant; lubOf (genquant &&. (genquant &&. single_data)) =. model;
                           lubOf (genquant &&. (genquant &&. single_model)) =. genquant; lubOf (model &&. (genquant &&. single_genquant)) =. genquant; lubOf (genquant &&. (model &&. single_genquant)) =. genquant;
                           lubOf (model &&. (genquant &&. single_model)) =. genquant; lubOf (genquant &&. (model &&. single_model)) =. genquant; lubOf (model &&. (model &&. single_genquant)) =. genquant;
                           lubOf (genquant &&. (data &&. single_model)) =. genquant; lubOf (model &&. (data &&. single_genquant)) =. genquant; 
                           lubOf (data &&. (model &&. single_genquant)) =. genquant; lubOf (genquant &&. (model &&. single_model)) =. genquant;
                           lubOf (genquant &&. (model &&. single_data)) =. genquant; lubOf (model &&. (genquant &&. single_data)) =. genquant;
                           lubOf (model &&. (model &&. single_model)) =. model; lubOf (genquant &&. (genquant &&. single_genquant)) =. model;

                           q_lub ]
                        |> List.fold (fun s c -> context.MkAnd(s, c)) (context.MkTrue())
                        |> fun e -> e.Simplify() :?> BoolExpr

    //////////////////////////////

    let body = glbOf (ell &&. ells) =. glbOf (ell &&. (glbOf ells &&. nil))

    let q_glb = context.MkForall([| Level :> Sort; levlist :> Sort |], 
                                 [| ell_symbol ; ells_symbol |], 
                                 body) :> BoolExpr

    let glb_definition = [ glbOf nil =. genquant; glbOf single_data =. data; 
                           glbOf single_model =. model; glbOf single_genquant =. genquant; 
                           glbOf (data &&. single_data) =. data; glbOf (data &&. single_model) =. data; 
                           glbOf (data &&. single_genquant) =. data; glbOf (model &&. single_model) =. model;
                           glbOf (model &&. single_genquant) =. model; glbOf (model &&. single_data) =. data;
                           glbOf (genquant &&. single_genquant) =. genquant; glbOf (genquant &&. single_model) =. model;
                           glbOf (genquant &&. single_data) =. data;
                           q_glb ]
                        |> List.fold (fun s c -> context.MkAnd(s, c)) (context.MkTrue())
                        |> fun e -> e.Simplify() :?> BoolExpr

    let translated = List.map translate constraints

    solver.Assert(leq_definition)
    solver.Assert(lub_definition)
    solver.Assert(glb_definition)
    
    for i in 0 .. List.length translated - 1 do 
        let label = context.MkBoolConst(sprintf "C%A" i)
        solver.Assert(List.item i translated) //, label) // AssertAndTrack

    for k, l in Map.toList level_vars do
        let _ = solver.AssertSoft(l =. data, uint32 100, ":weight")
        let _ = solver.AssertSoft(l =. model, uint32 0, ":weight")
        let _ = solver.AssertSoft(l =. genquant, uint32 10, ":weight")
        ()
        
    let mapping =
        match solver.Check() with
        | Status.SATISFIABLE ->
            let m = solver.Model
            Map.map (
                fun k l ->  
                    match m.Evaluate(l).ToString() with 
                    | "data" -> Data 
                    | "model" -> Model 
                    | "genquant" -> GenQuant
                    | _ -> failwith "unexpected" 
            ) level_vars 

        | _ -> 
            let unsat = solver.UnsatCore
            let index = sprintf "%A" unsat.[0]
                     |> fun x -> System.Int32.TryParse (x.[1..1]) |> snd
            
            let problematic_constraint = List.item index constraints_info

            failwith (sprintf "Could not resolve constraints! %A" (snd problematic_constraint))

    context.Dispose()

    mapping



let resolve_semilattice (constraints_info : ConstraintInfo list, level_vars_names : string list) = 

    let constraints = List.map fst constraints_info

    use context = new Context()
    let solver = context.MkOptimize() //MkSolver() //.

    let l1_constr = context.MkConstructor("l1", "is_l1")
    let l2_constr = context.MkConstructor("l2", "is_l2")
    let l3_constr = context.MkConstructor("l3", "is_l3")
    let err_constr = context.MkConstructor("err", "is_err")
    let constructors = [| l1_constr; l2_constr; l3_constr; err_constr |]

    let Level : DatatypeSort = context.MkDatatypeSort("ell", constructors)
    
    let s : Sort [] = [| Level; Level |]
    let levlist : ListSort = context.MkListSort("levlist", Level)
    let leq : FuncDecl = context.MkFuncDecl("leq", s, context.BoolSort) 
    let lub : FuncDecl = context.MkFuncDecl("lub", levlist, Level) 
    let glb : FuncDecl = context.MkFuncDecl("glb", levlist, Level) 

    let (<=.) (x: Expr) (y: Expr) = context.MkApp(leq, [| x; y |]) :?> BoolExpr
    let (=.)  (x: Expr) (y: Expr) = context.MkEq(x, y)
    let nott  (x: BoolExpr)       = context.MkNot(x)
    let lubOf (x: Expr)           = context.MkApp(lub, x)
    let glbOf (x: Expr)           = context.MkApp(glb, x)
    let (&&.) (x: Expr) (y: Expr) = context.MkApp(levlist.ConsDecl, x, y)

    let l1 = context.MkConst(l1_constr.ConstructorDecl)
    let l2 = context.MkConst(l2_constr.ConstructorDecl) 
    let l3 = context.MkConst(l3_constr.ConstructorDecl)   
    let err = context.MkConst(err_constr.ConstructorDecl)   

    let f (l_name : string) : (string * Expr) = l_name, context.MkConst(l_name, Level)
    let level_vars = List.map f level_vars_names |> Map.ofList

    let nonIsErr : BoolExpr = 
        let lvars = Map.toList level_vars |> List.map snd

        if List.length lvars = 0 then context.MkTrue()
        else List.fold (fun s x -> context.MkAnd(s, x =. err |> nott)) ((List.head lvars) =. err |> nott) (List.tail lvars) 
            

    let rec translate_level (ell : TypeLevel) : Expr =
        match ell with 
        | Data -> l1
        | Model -> failwith "unexpected! no model level for CI analysis" //l1
        | Lz -> l2
        | GenQuant -> l3
        | LevelVar x -> level_vars.Item(x)
        | Lub ells -> 
            let tr = List.map translate_level ells
                    |> List.fold (fun s el -> context.MkApp(levlist.ConsDecl, el, s)) (context.MkConst(levlist.NilDecl))                   
            context.MkApp(lub, tr)
        | Glb ells -> 
            let tr = List.map translate_level ells
                    |> List.fold (fun s el -> context.MkApp(levlist.ConsDecl, el, s)) (context.MkConst(levlist.NilDecl))                   
            context.MkApp(glb, tr)

    let translate (c : Constraint) : BoolExpr = 
        match c with
        | Leq(lev1, lev2) ->
            let new1 = translate_level lev1
            let new2 = translate_level lev2
            context.MkAnd( new1 <=. new2,  context.MkAnd(new1 =. err |> nott, new2 =. err |> nott))
            //(new1 <=. new2).Simplify() :?> BoolExpr

    let leq_definition = [ l1 <=. l1; l1 <=. l2; l1 <=. l3;
                           nott (l2 <=. l1); l2 <=. l2; nott(l2 <=. l3);
                           nott (l3 <=. l1); nott (l3 <=. l2); l3 <=. l3;
                           nott (err <=. l1); nott (err <=. l2); nott (err <=. l3);
                           nott (l1 <=. err); nott (l2 <=. err); nott (l3 <=. err); 
                           nott (err <=. err);]
                        
                        |> List.fold (fun s c -> context.MkAnd(s, c)) (context.MkTrue())
                        |> fun e -> e.Simplify() :?> BoolExpr


    let nil = context.MkConst(levlist.NilDecl)
    let single_l1 = l1 &&. nil
    let single_l2 = l2 &&. nil
    let single_l3 = l3 &&. nil
    let single_err = err &&. nil

    let ell_symbol = context.MkSymbol("ell") :> Symbol
    let ell = context.MkConst(ell_symbol, Level)
    let ells_symbol = context.MkSymbol("ells") :> Symbol
    let ells = context.MkConst(ell_symbol, levlist)

    let body_base = lubOf (ell &&. nil) =. ell
    let body = lubOf (ell &&. ells) =. lubOf (ell &&. ((lubOf ells) &&. nil))

    let q_lub = context.MkForall([| Level :> Sort; levlist :> Sort |], 
                                 [| ell_symbol ; ells_symbol |], 
                                 context.MkAnd(body_base, body)) :> BoolExpr

    let lub_definition = [ lubOf nil =. l1; lubOf single_l1 =. l1; lubOf single_l2 =. l2; 
                           lubOf single_l3 =. l3; lubOf single_err =. err;
                           lubOf (l1 &&. single_l1) =. l1; lubOf (l1 &&. single_l2) =. l2; 
                           lubOf (l1 &&. single_l3) =. l3; lubOf (l2 &&. single_l2) =. l2;
                           lubOf (l2 &&. single_l1) =. l2; lubOf (l3 &&. single_l3) =. l3; 
                           lubOf (l3 &&. single_l1) =. l3;                        
                           lubOf (l2 &&. single_l3) =. err; lubOf (l3 &&. single_l2) =. err;
                           lubOf (err &&. single_l1) =. err; lubOf (err &&. single_l2) =. err; 
                           lubOf (err &&. single_l3) =. err; lubOf (err &&. single_err) =. err; 
                           lubOf (l1 &&. single_err) =. err; lubOf (l2 &&. single_err) =. err;
                           lubOf (l3 &&. single_err) =. err;

                           // FIXME: the q_lub bit of the theory doesn't seem to work...
                           // Adding the following as a way to solve some of the current 
                           // examples we have, but should fix to be more general.
                           lubOf (l1 &&. (l1 &&. single_l2)) =. l2; lubOf (l2 &&. (l1 &&. single_l1)) =. l2;
                           lubOf (l2 &&. (l1 &&. single_l2)) =. l2; lubOf (l2 &&. (l1 &&. single_l2)) =. l2;
                           lubOf (l3 &&. (l1 &&. single_l2)) =. err; lubOf (l2 &&. (l1 &&. single_l3)) =. err;
                           lubOf (l3 &&. (l2 &&. single_l1)) =. err; lubOf (l2 &&. (l3 &&. single_l1)) =. err;

                           q_lub ]

                        |> List.fold (fun s c -> context.MkAnd(s, c)) (context.MkTrue())
                        |> fun e -> e.Simplify() :?> BoolExpr

    //////////////////////////////

    let body_base = glbOf (ell &&. nil) =. ell
    let body = glbOf (ell &&. ells) =. glbOf (ell &&. (glbOf ells &&. nil))

    let q_glb = context.MkForall([| Level :> Sort; levlist :> Sort |], 
                                 [| ell_symbol ; ells_symbol |], 
                                 context.MkAnd(body_base, body)) :> BoolExpr

    let glb_definition = [ glbOf single_l1 =. l1; glbOf single_l2 =. l2; glbOf single_l3 =. l3; 
                           glbOf (l1 &&. single_l1) =. l1; glbOf (l1 &&. single_l2) =. l1; 
                           glbOf (l1 &&. single_l3) =. l1; glbOf (l2 &&. single_l2) =. l2;
                           glbOf (l2 &&. single_l3) =. l1; glbOf (l2 &&. single_l1) =. l1;
                           glbOf (l3 &&. single_l3) =. l3; glbOf (l3 &&. single_l2) =. l1;
                           glbOf (l3 &&. single_l1) =. l1;
                           glbOf (err &&. single_l1) =. err; glbOf (err &&. single_l2) =. err; 
                           glbOf (err &&. single_l3) =. err; glbOf (err &&. single_err) =. err; 
                           glbOf (l1 &&. single_err) =. err; glbOf (l2 &&. single_err) =. err;
                           glbOf (l3 &&. single_err) =. err; glbOf single_err =. err;
                           q_glb ]
                        |> List.fold (fun s c -> context.MkAnd(s, c)) (context.MkTrue())
                        |> fun e -> e.Simplify() :?> BoolExpr

    let translated = List.map translate constraints
                   //|> List.fold (fun s c -> context.MkAnd(s, c)) (context.MkTrue())
                   //|> fun e -> e.Simplify() :?> BoolExpr

  
    solver.Assert(leq_definition)
    solver.Assert(lub_definition)
    solver.Assert(glb_definition)
    solver.Assert(nonIsErr)
    
    for i in 0 .. List.length translated - 1 do 
        let label = context.MkBoolConst(sprintf "C%A" i)
        solver.Assert(List.item i translated)// , label) //.AssertAndTrack
        
    for k, l in Map.toList level_vars do
        let _ = solver.AssertSoft(l =. l1, uint32 10,   ":weight")
        let _ = solver.AssertSoft(l =. l2, uint32 0,   ":weight")
        let _ = solver.AssertSoft(l =. l3, uint32 100,  ":weight")
        ()

    let mapping =
        match solver.Check() with
        | Status.SATISFIABLE ->
            let m = solver.Model
            let ret = Map.map (
                        fun k l ->  
                            let mstr = m.Evaluate(l).ToString()
                            match mstr with 
                            | "l1" -> Model 
                            | "l2" -> Lz 
                            | "l3" -> GenQuant
                            | _ -> failwith "unexpected! can't resolve level type to err" 
                        ) level_vars
            ret

        | x -> 
            let unsat = solver.UnsatCore
            let index = sprintf "%A" unsat.[0]
                     |> fun x -> System.Int32.TryParse (x.[1..1]) |> snd
            
            let problematic_constraint = List.item index constraints_info

            failwith (sprintf "Could not resolve constraints! %A" (snd problematic_constraint))

    context.Dispose()

    mapping
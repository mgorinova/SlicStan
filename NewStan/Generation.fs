module Generation

#if INTERACTIVE
#I __SOURCE_DIRECTORY__
#r "bin\Debug\NewStan.exe"
#endif

open NewStanSyntax
open MiniStanSyntax
open System

let rnd_s = System.Random()
let rnd_a = System.Random()
let rnd_v = System.Random()

let typeLevelNames = NewStanSyntax.typeLevelNames
let varNames = Seq.initInfinite (fun index -> if (index < 123) then Char.ConvertFromUtf32(index) 
                                              elif (index < 701) then 
                                                let s1 = Char.ConvertFromUtf32(index/26 + 96)
                                                let s2 = Char.ConvertFromUtf32(index%26 + 97)
                                                let name = s1 + s2 
                                                if (name = "if" || name = "in") then "i" + name
                                                else name

                                              else 
                                                let s1 = Char.ConvertFromUtf32(index/676 + 96)
                                                let s2 = Char.ConvertFromUtf32((index/26)%26 + 96)
                                                let s3 = Char.ConvertFromUtf32(index%26 + 97)
                                                s1 + s2 + s3 )
                                              

let mutable curvar = 97
let nextvar() =
    let ret = Seq.nth curvar varNames
    curvar <- curvar + 1
    ret

let getrandom_v (list: string[]) =  
  list.[rnd_v.Next(list.Length)]

let getrandom_s (list: string[]) =  
  list.[rnd_s.Next(list.Length)]

let getrandom_a (list: string[]) =  
  list.[rnd_a.Next(list.Length)]

let unary_functions = [|"sqrt"; "exp"; "log"|]
let binary_functions = [|"+"; "-"; "*"; "/"; "pow"|]

let dists = [|"normal"; "gamma"; "cauchy"; "beta"; "uniform"|]

let actions = [|"sample"; "assign"; "assign" |]
let sequences = [|"seq"; "seq"; "seq"; "seq"; "seq"; "seq"; "seq"; "seq"; "seq"; "data"; "block"; "block"|]

let mutable datavars : string list = []
let mutable vars : string list = []

let both () =
    List.append datavars vars

let no s list =
    Array.filter (fun name -> not (name = s)) (Array.ofList list)

let generate_S () =
    match getrandom_s actions with 
    | "assign" -> 
        match getrandom_s [|"unary"; "binary"; "ecall"|] with //; "ecall"|] with
        | "unary" ->
            if (List.length vars < 2) then Skip else
                let name = getrandom_s unary_functions
                let x1 = getrandom_v (Array.ofList vars)
                let x2 = getrandom_v (no x1 (both()))           
                Assign(I(x1), Prim(name, [Var x2]))

        | "binary" -> 
            if (List.length vars < 3) then Skip else
                let name = getrandom_s binary_functions
                let x1 = getrandom_v (Array.ofList vars)
                let x2, x3 = getrandom_v (no x1 (both())), getrandom_v (no x1 (both()))        
                Assign(I(x1), Prim(name, [Var x2; Var x3]))

        | "ecall" -> 
            if (List.length vars < 3) then Skip else
                let x1 = getrandom_v (Array.ofList vars)
                let x2, x3 = getrandom_v (no x1 (both())), getrandom_v (no x1 (both()))        
                Assign(I(x1), ECall("my_normal", [Var x2; Var x3]))
      
    | "sample" -> 
        if (List.length vars < 3) then Skip else
            let name = getrandom_s dists
            let x1 = getrandom_v (Array.ofList vars)
            let x2, x3 = getrandom_v (no x1 (both())), getrandom_v (no x1 (both()))          
            NewStanSyntax.Sample(x1, Dist(name, [Var x2; Var x3]))


let rec generate_manyS (n:int) =
    if n < 1 then Skip
    else 
        match getrandom_a sequences with
        | "seq" -> 
            let s = generate_S ()
            Seq(s, generate_manyS (n-1))
        | "data" -> 
            let name = nextvar()
            datavars <- name::datavars
            DataDecl(Real, name, generate_manyS (n-1))
        | "block" -> 
            let name = nextvar()
            vars <- name::vars
            let level = next()
            Block(((Real, LevelVar(level)), name), generate_manyS (n-1))

/// Generates statistics for the main body of a program S
/// returns (num_lines, num_datadecl, num_decl, num_assignments, num_sampling, num_ecalls)
let stats (S:S) =     
    let rec stats_e e n =
        match e with
        | ECall _ -> n + 1
        | _ -> n
    
    let rec stats_rec s (num_lines, num_datadecl, num_decl, num_assignments, num_sampling, num_ecalls) : int*int*int*int*int*int =
        match s with
        | DataDecl (_, _, s') -> stats_rec s' (num_lines + 1, num_datadecl + 1, num_decl, num_assignments, num_sampling, num_ecalls)
        | Block (_, s') -> stats_rec s' (num_lines + 1, num_datadecl, num_decl + 1, num_assignments, num_sampling, num_ecalls)
        | Assign (_,e) -> 
            let n = stats_e e 0
            (num_lines + 1, num_datadecl, num_decl, num_assignments + 1, num_sampling, num_ecalls + n)
        | NewStanSyntax.Sample _ -> (num_lines + 1, num_datadecl, num_decl, num_assignments, num_sampling + 1, num_ecalls)
        | Seq (s1, s2) -> 
            let (n1, n2, n3, n4, n5, n6) = stats_rec s1 (num_lines, num_datadecl, num_decl, num_assignments, num_sampling, num_ecalls)
            stats_rec s2 (n1, n2, n3, n4, n5, n6)
        | Skip -> (num_lines, num_datadecl, num_decl, num_assignments, num_sampling, num_ecalls)
        | VCall _ -> (num_lines + 1, num_datadecl, num_decl, num_assignments, num_sampling, num_ecalls)

    stats_rec S (0,0,0,0,0,0)


/// Returns NumbDecls(d, td, p, tp, m, gq) * num_lines, num_assignments, num_sampling
let stan_stats (P(d, td, p, tp, m, gq) : MiniStanSyntax.Prog) = 
    
    let rec decl_count (decls : VarDecls) : int = 
        match decls with 
        | VNone -> 0
        | Declr _ -> 1
        | VSeq (ds1, ds2) -> (decl_count ds1) + (decl_count ds2)        

    let rec s_count (ss : Statements) : int * int * int = 
        match ss with
        | Let _ -> (1, 1, 0)
        | MiniStanSyntax.Sample _ -> (1, 0, 1)
        | SSeq (s1, s2)-> 
            let n1, n2, n3 = s_count s1
            let n1', n2', n3' = s_count s2
            n1+n1', n2+n2', n3+n3'
        | SNone -> (0,0,0)
    
    let stats_d = match d with 
                  | DBlock decls -> decl_count decls
                  | DNone -> 0

    let stats_td, tdcommon = match td with 
                                | TDBlock (decls, ss) -> (decl_count decls), (s_count ss)
                                | TDNone -> 0, (0, 0, 0)
    
    let stats_p = match p with 
                  | PBlock decls -> decl_count decls
                  | PNone -> 0


    let stats_tp, tpcommon = match tp with 
                             | TPBlock (decls, ss) -> (decl_count decls), (s_count ss)
                             | TPNone -> 0, (0, 0, 0)

    let stats_m, mcommon = match m with 
                           | MBlock (decls, ss) -> (decl_count decls), (s_count ss)

    let stats_gq, gqcommon = match gq with 
                             | GQBlock (decls, ss) -> (decl_count decls), (s_count ss)
                             | GQNone -> 0, (0, 0, 0)

    let decls_all = (stats_d, stats_td, stats_p, stats_tp, stats_gq)

    let common_all = [tdcommon; tpcommon; mcommon; gqcommon]

    let common_merged = List.fold (fun (i, j, k) (i', j', k') -> (i+i', j+j', k+k') ) (0,0,0) common_all

    decls_all, common_merged
    
 
//let ex : NewStanSyntax.NewStanProg = [], generate_manyS 100
//printfn "%s" (NewStanSyntax.NewStanProg_pretty ex)


let reset() =
    NewStanSyntax.reset_levels()
    curvar <- 97
    datavars <- []
    vars <- []
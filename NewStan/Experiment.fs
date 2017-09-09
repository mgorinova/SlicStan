module Experiment

#if INTERACTIVE
#I __SOURCE_DIRECTORY__
#r "bin\Debug\NewStan.exe"
#endif

open Generation
open Typecheck
open Elaborate
open Translate

open System.Diagnostics;



type SingleStats = (int64*int64*int64)*(int*int*int*int*int*int)*((int*int*int*int*int)*(int*int*int))
type Stats = {inference_times :int64 list; elaboration_time:int64 list; translation_time: int64 list;
              slic_lines:int list; slic_datadecl:int list; slic_decl:int list; slic_assignments:int list; slic_sampling:int list; slic_ecalls:int list;
              data:int list; trans_data:int list; paramss:int list; trans_params:int list; gen_quants:int list;
              stan_lines:int list; stan_assignments:int list; stan_sampling:int list}

let parse slicstan = 
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<char>.FromString slicstan
    let res = Parser.start Lexer.read lexbuf
    res


// Returns:
// (inference_time, elaboration_time, translation_time) *
// (num_lines, num_datadecl, num_decl, num_assignments, num_sampling, num_ecalls) *
// (NumbDecls(d, td, p, tp, m, gq) * (num_lines, num_assignments, num_sampling))
let run_single fs n : SingleStats = 
    Generation.reset()
    let s = generate_manyS n
    let ex = fs, s

    let stopwatch = new Stopwatch()
    
    stopwatch.Start()
    let inferred = typecheck_Prog ex 
    let inference_time = stopwatch.ElapsedMilliseconds

    stopwatch.Restart()
    let context, elab = Elaborate.elaborate_NewStanProg inferred
    let elaboration_time = stopwatch.ElapsedMilliseconds //

    stopwatch.Restart()
    let translated = (Translate.translate context elab)
    let translation_time = stopwatch.ElapsedMilliseconds

    (inference_time, elaboration_time, translation_time), (Generation.stats s), (Generation.stan_stats translated)




let empty_stats: Stats = {inference_times=[]; elaboration_time=[]; translation_time=[];
                          slic_lines=[]; slic_datadecl=[]; slic_decl=[]; slic_assignments=[]; slic_sampling=[]; slic_ecalls=[];
                          data=[]; trans_data=[]; paramss=[]; trans_params=[]; gen_quants=[];
                          stan_lines=[]; stan_assignments=[]; stan_sampling=[]}


let concat (stats: SingleStats) (record: Stats) =
    let (inference_time, elaboration_time, translation_time),
        (slic_lines, slic_datadecl, slic_decl, slic_assignments, slic_sampling, slic_ecalls),
        ((d, td, p, tp, gq), (num_lines, num_assignments, num_sampling)) = stats 

    {inference_times=inference_time::record.inference_times; 
     elaboration_time=elaboration_time::record.elaboration_time; 
     translation_time=translation_time::record.translation_time;
     slic_lines=slic_lines::record.slic_lines; slic_datadecl=slic_datadecl::record.slic_datadecl; 
     slic_decl=slic_decl::record.slic_decl; slic_assignments=slic_assignments::record.slic_assignments;
     slic_sampling=slic_sampling::record.slic_sampling; slic_ecalls=slic_ecalls::record.slic_ecalls;
     data=d::record.data; trans_data=td::record.trans_data; paramss=p::record.paramss;
     trans_params=tp::record.trans_params; gen_quants=gq::record.gen_quants;
     stan_lines=num_lines::record.stan_lines; stan_assignments=num_assignments::record.stan_assignments; 
     stan_sampling=num_sampling::record.stan_sampling}


let multiple_runs fs size runs =
    
    let mutable acc = empty_stats
    for i in 1..runs do
        acc <- concat (run_single fs size) acc
    acc
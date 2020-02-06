// Learn more about F# at http://fsharp.org

open System
open System.IO

open HccFS.Ksexp
open HccFS.Compiler
open HccFS.KinseiCompiler

[<EntryPoint>]
let main argv =
  Array.iter (fun fileName ->
    let fileData = File.ReadAllText fileName
    printfn "fileName: %s" fileName
    printfn "%s" fileData
    let parsed = sexpParse fileData
    printfn "parsed: %A" parsed
    let compiled = compile parsed
    printfn "compiled: %A" compiled
    let kinseiCode = CompileToKinseiCode compiled @ [Hlt]
    printfn "kinsei code:"
    List.iter (fun code -> printfn "%s" (code.ToString())) kinseiCode
    let baseName = Path.GetFileNameWithoutExtension fileName
    let dstName = sprintf "%s.kasm" baseName

    File.WriteAllLines (dstName, (List.map (fun c -> c.ToString()) kinseiCode))
    printfn "[Assembly codes are saved as %s]" dstName
    ) argv
  0 // return an integer exit code

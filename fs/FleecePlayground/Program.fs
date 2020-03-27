// Learn more about F# at http://fsharp.org

open FSharpPlus
open Fleece.Newtonsoft
open Fleece.Newtonsoft.Operators

type JsonOption<'a> = JsonOption of 'a option with
  static member inline ToJson (JsonOption opt) = 
    match opt with
    | None -> JNull
    | Some x -> toJson x

  static member inline OfJson json = 
    match json with
    | JNull -> Success (JsonOption None)
    | anythingElse -> 
      monad {
        let! x = ofJson anythingElse
        return JsonOption (Some x)
      }

let inline objParseError json = Failure (sprintf "Expecting an object.  Got %s" (string json))

let asObj = function
  | JObject o -> Success o
  | json -> objParseError json

let inline defaultObjOfJson1 json constructor field = 
  monad {
    let! o = asObj json
    let! f = o .@ field
    return constructor f
  }

let inline defaultObjOfJson2 json constructor field1 field2 = 
  monad {
    let! o = asObj json
    let! f1 = o .@ field1
    let! f2 = o .@ field2
    return constructor f1 f2
  }

let inline defaultObjOfJson3 json constructor field1 field2 field3 = 
  monad {
    let! o = asObj json
    let! f1 = o .@ field1
    let! f2 = o .@ field2
    let! f3 = o .@ field3
    return constructor f1 f2 f3
  }

type Sample = 
  { 
    X : int JsonOption
    Y : int JsonOption
  }
  with
  static member ToJson { X = x; Y = y } = jobj [ ("x", toJson x); ("y", toJson y) ]
  static member OfJson json = defaultObjOfJson2 json (fun x y -> {X=x; Y=y}) "x" "y"

[<EntryPoint>]
let main argv =
  let fsVersion = { X = JsonOption (Some 3); Y = JsonOption None }
  let json = toJson fsVersion
  let rt : Sample ParseResult = ofJson json

  printfn "json str: \"%s\"" (string json)
  printfn "round-trip object: %A" rt
  
  printfn "bad Sample: %A" (parseJson """ {"x":3.0, "z":5.0} """ : Sample ParseResult)

  0 // return an integer exit code




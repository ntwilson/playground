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

let inline defaultObjOfJson1 constructor field json = 
  match json with 
  | JObject o -> constructor <!> (o .@ field)  
  | _ -> Failure (sprintf "Expecting an object.  Got %s" (string json))
    
let inline defaultObjOfJson2 constructor field1 field2 json = 
  match json with 
  | JObject o -> constructor <!> (o .@ field1) <*> (o .@ field2)
  | _ -> Failure (sprintf "Expecting an object.  Got %s" (string json))
    
let inline defaultObjOfJson3 constructor field1 field2 field3 json = 
  match json with 
  | JObject o -> constructor <!> (o .@ field1) <*> (o .@ field2) <*> (o .@ field3)
  | _ -> Failure (sprintf "Expecting an object.  Got %s" (string json))
    
type Sample = 
  { 
    X : int JsonOption
    Y : int JsonOption
  }
  with
  static member ToJson { X = x; Y = y } = jobj [ ("x", toJson x); ("y", toJson y) ]
  static member OfJson json = defaultObjOfJson2 (fun x y -> {X=x; Y=y}) "x" "y" json

[<EntryPoint>]
let main argv =
  let fsVersion = { X = JsonOption (Some 3); Y = JsonOption None }
  let json = toJson fsVersion
  let rt : Sample ParseResult = ofJson json

  printfn "json str: \"%s\"" (string json)
  printfn "round-trip object: \"%A\"" rt
  0 // return an integer exit code




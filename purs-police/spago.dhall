{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "purs-police"
, dependencies =
    [ 
        "effect", 
        "console", 
        "prelude", 
        "ordered-collections", 
        "foldable-traversable",
        "exceptions", 
        "infinite-lists"
    ]
, packages =
    ./packages.dhall
}

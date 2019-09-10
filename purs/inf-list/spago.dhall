{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "inf-list"
, dependencies =
    [ "effect", "console", "psci-support", "lists", "spec" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}

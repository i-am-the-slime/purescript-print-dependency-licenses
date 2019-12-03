{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "purescript-print-dependency-licenses"
, dependencies =
    [ "aff"
    , "avar"
    , "console"
    , "debug"
    , "effect"
    , "kishimen"
    , "node-child-process"
    , "node-fs"
    , "node-process"
    , "parsing"
    , "psci-support"
    , "simple-json"
    ]
, packages =
    ./packages.dhall
}

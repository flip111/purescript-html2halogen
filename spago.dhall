{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "html2halogen"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "language-cst-parser"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "optparse"
  , "partial"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tidy-codegen"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-dom-parser"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

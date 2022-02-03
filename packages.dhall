{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220110/packages.dhall sha256:8dbf71bfc6c7a11043619eebe90ff85f7d884541048aa8cc48eef1ee781cbc0e
  with tidy-codegen.repo = "https://github.com/natefaubion/purescript-tidy-codegen.git"
  with tidy-codegen.version = "main"
  with tidy-codegen.dependencies =
    [ "aff"
    , "ansi"
    , "arrays"
    , "avar"
    , "bifunctors"
    , "console"
    , "control"
    , "dodo-printer"
    , "effect"
    , "either"
    , "enums"
    , "exceptions"
    , "filterable"
    , "foldable-traversable"
    , "free"
    , "identity"
    , "integers"
    , "language-cst-parser"
    , "lazy"
    , "lists"
    , "maybe"
    , "newtype"
    , "node-buffer"
    , "node-child-process"
    , "node-fs-aff"
    , "node-path"
    , "node-process"
    , "node-streams"
    , "ordered-collections"
    , "parallel"
    , "partial"
    , "posix-types"
    , "prelude"
    , "record"
    , "safe-coerce"
    , "strings"
    , "tidy"
    , "transformers"
    , "tuples"
    , "type-equality"
    , "unicode"
    ]

let overrides = {=}

let additions =
  { dodo-printer =
    { dependencies =
      [ "ansi", "foldable-traversable", "lists", "maybe", "strings" ]
    , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
    , version = "v2.1.0"
    }
  , language-cst-parser =
    { dependencies =
      [ "arrays"
      , "const"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "free"
      , "functors"
      , "maybe"
      , "numbers"
      , "ordered-collections"
      , "strings"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      ]
    , repo = "https://github.com/natefaubion/purescript-language-cst-parser.git"
    , version = "v0.9.1"
    }
  , tidy =
    { dependencies =
      [ "arrays"
      , "dodo-printer"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "language-cst-parser"
      , "strings"
      , "tuples"
      ]
    , repo = "https://github.com/natefaubion/purescript-tidy.git"
    , version = "v0.5.3"
    }
  , web-dom =
    { dependencies =
      [ "effect"
      , "enums"
      , "maybe"
      , "newtype"
      , "nullable"
      , "prelude"
      , "unsafe-coerce"
      , "web-events"
      ]
    , repo = "https://github.com/flip111/purescript-web-dom.git"
    , version = "patch-1"
    }
  }

in  upstream // overrides // additions

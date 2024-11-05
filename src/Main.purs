module Main where

import Prelude

import Control.Monad.State
import Data.Array hiding (fold, length, null, foldM, elem, notElem, intercalate)
import Data.Either
import Data.Foldable
import Data.Generic.Rep (class Generic)
import Data.Int
import Data.Maybe
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String hiding (null, length)
import Data.Traversable
import Data.Tuple
import Effect (Effect)
import Effect.Class
import Effect.Console (log, error)
import Foreign.Object as FO
import Node.Encoding
import Node.FS.Sync
import Options.Applicative
import Options.Applicative.Builder (eitherReader)
import Partial.Unsafe
import PureScript.CST.Print
import PureScript.CST.Types
import Tidy.Codegen
import Web.DOM.Attr (Attr)
import Web.DOM.Attr as Attr
import Web.DOM.AttrName
import Web.DOM.ClassName
import Web.DOM.Document hiding (fromNode)
import Web.DOM.DOMParser
import Web.DOM.DOMTokenList hiding (length)
import Web.DOM.Element hiding (toNode)
import Web.DOM.ElementId
import Web.DOM.ElementName
import Web.DOM.NamedNodeMap hiding (length)
import Web.DOM.NamespacePrefix
import Web.DOM.NamespaceURI
import Web.DOM.Node
import Web.DOM.NodeList hiding (length)
import Web.DOM.NodeType


foreign import makeNodeCompatible :: Effect Unit
foreign import elementGetOuterHtml :: Element -> String

data CLArgs = CLArgs
  { inputFilepath :: String
  , outputFilepath :: String
  , noCssHelper :: Boolean
  , format :: CLFormat
  }
derive instance genericCLArgs :: Generic CLArgs _
instance showCLArgs :: Show CLArgs where
  show = genericShow

formatHelp :: String
formatHelp
  =  "format controls the contents of the resulting purescript file.\n"
  <> "\n"
  <> "plain: simplest type of output with most flexible HTML type. Suitable to copy paste into your own code.\n"
  <> "file: when executed renders the html and saves it to a file. Uses nodejs dependencies. Useful for testing html2halogen.\n"
  <> "raw: Uses halogen's type ComponentHTML which allows injection of HTML as a raw string. Useful for encountering unsupported HTML elements.\n"
  <> "app: Same as raw but comes with extra code that allows the code executed as halogen app in the browser. (parcel index.html)"

data CLFormat = Plain | File | Raw | App
derive instance eqCLFormat :: Eq CLFormat
derive instance genericCLFormat :: Generic CLFormat _
instance showCLFormat :: Show CLFormat where
  show = genericShow

parseFormat :: ReadM CLFormat
parseFormat = eitherReader $ \s -> case (toLower s) of
  "plain" -> Right Plain
  "file"  -> Right File
  "raw"   -> Right Raw
  "app"   -> Right App
  _       -> Left $ "format should be either one of plain, file, raw or app. Found \"" <> s <> "\""

cl_args_parser :: Parser CLArgs
cl_args_parser = ado
  inputFilepath <- strOption $ fold
    [ long "input"
    , metavar "FILEPATH"
    , help "input html file"
    ]

  outputFilepath <- strOption $ fold
    [ long "output"
    , metavar "FILEPATH"
    , help "output html file"
    ]

  noCssHelper <- switch
    (  long "no-css-helper"
    <> help "Disable the css helper function and use coerce for ClassName newtype wrapper." )

  format <- option parseFormat
    ( long "format"
    <> metavar "FORMAT"
    <> help formatHelp )

  in CLArgs { inputFilepath, outputFilepath, noCssHelper, format }

cliOptions ∷ ParserInfo CLArgs
cliOptions = info (cl_args_parser <**> helper)
  ( fullDesc
 <> header "html2halogen" )

type RunState =
  { errors :: Array String -- Doesn't need to be read during execution
  , waitForBody :: Boolean -- "</head> <body>" will yield one single space #text element, which we want to ignore. This flag helps
  , sawClasses :: Boolean -- the test html doesn't actually use classes so we have an unsed import Unsafe.Coerce
  , noCssHelper :: Boolean -- this is not supposed to be written to, possibly should be ReaderT
  }
initialRunState :: CLArgs -> RunState
initialRunState (CLArgs cli_options) =
  { errors: []
  , waitForBody: false
  , sawClasses: false
  , noCssHelper: cli_options.noCssHelper
  }


getChildrenExpr :: Partial => CLArgs -> Node -> StateT RunState Effect (Array (Array (Expr Void)))
getChildrenExpr clargs n = do
  child_nodes <- liftEffect $ childNodes n >>= toArray
  traverse (run clargs) child_nodes

addInvalidHTMLError :: String -> StateT RunState Effect Unit
addInvalidHTMLError err = addError $ "error[invalid HTML] " <> err

addUnimplementedError :: String -> StateT RunState Effect Unit
addUnimplementedError err = addError $ "error[unimplemented] " <> err

addConvertedToRawInfo :: String -> StateT RunState Effect Unit
addConvertedToRawInfo err = addError $ "info[converted to raw] " <> err

addNoHalogenError :: String -> StateT RunState Effect Unit
addNoHalogenError err = addError $ "error[no halogen equivalent] " <> err

addProgramError :: String -> StateT RunState Effect Unit
addProgramError err = addError $ "error[program error] " <> err

addError :: String -> StateT RunState Effect Unit
addError err = modify_ \rec@{ errors } -> rec { errors = errors <> [err] }

addContinueError :: String -> StateT RunState Effect Unit
addContinueError node_name = addNoHalogenError $ "Found node \"" <> node_name <> "\", continuing with it's child nodes."


n_html :: Partial => CLArgs -> Node -> StateT RunState Effect (Array (Expr Void))
n_html clargs n = do
  modify_ (_ { waitForBody = true })
  (liftEffect $ hasChildNodes n) >>= case _ of
    false -> do
      addNoHalogenError "Found node \"html\" with no children, this is probably the doctype."
      pure []
    true  -> errorAndContinue clargs n


errorAndContinue :: Partial => CLArgs -> Node -> StateT RunState Effect (Array (Expr Void))
errorAndContinue clargs n = do
  addContinueError $ nodeName n
  children_expr <- getChildrenExpr clargs n

  if nodeName n `elem` ["HTML", "#document"] then
    pure $ concat children_expr
  else
    mergeChildren n children_expr

mergeChildren :: Node -> Array (Array (Expr Void)) -> StateT RunState Effect (Array (Expr Void))
mergeChildren n children_expr = foldM f [] children_expr
  where f :: Array (Expr Void) -> Array (Expr Void) -> StateT RunState Effect (Array (Expr Void))
        f acc childResult =
          case length childResult of
            0 -> pure acc
            1 -> pure $ acc <> [unsafePartial $ fromJust $ head childResult]
            _ -> do
              addProgramError $ "found a child that returned multiple elements in parent \"" <> (nodeName n) <> "\""
              pure acc


-- liftEffect $ getShortNodeInfo n >>= traceM
run :: Partial => CLArgs -> Node -> StateT RunState Effect (Array (Expr Void))
run clargs@(CLArgs cli_options) n = case nodeName n of
  "#document" -> do
    modify_ (_ { waitForBody = true })
    errorAndContinue clargs n
  "html" -> n_html clargs n
  "HTML" -> n_html clargs n
  "HEAD" -> do
    modify_ (_ { waitForBody = true })
    addNoHalogenError "Found node \"head\", skipping."
    pure []
  "BODY" -> do
    modify_ (_ { waitForBody = false })
    errorAndContinue clargs n
  "#text" -> do
    waitForBody <- gets _.waitForBody
    if waitForBody then
      pure []
    else do
      textContent <- liftEffect $ textContent n
      if String.null (trim textContent) then
        pure []
      else
        pure [exprApp (exprIdent "HH.text") [ exprString textContent ]]
  "SCRIPT" -> do
    handleNodeWithChildren n "script" $ \node -> do
      addUnimplementedError "traversing \"script\" children"
      pure $ [exprIdent "HH.script"]
  "CANVAS" -> do
    only_ws <- liftEffect $ noChildrenAndOnlyWhitespace n

    if only_ws then
      handleNode clargs n "canvas"
    else do
      addUnimplementedError "Handling of node \"canvas\" when it has children."
      pure []
  _ -> do
    let normal_elems = ["A", "ABBR", "ADDRESS", "ARTICLE", "AUDIO", "B", "BLOCKQUOTE", "BR", "BUTTON", "CAPTION", "circle", "CITE", "CODE", "DATALIST", "DD", "DEL", "DETAILS", "DFN", "DIV", "DL", "DT", "EM", "EMBED", "FIELDSET", "FIGCAPTION", "FIGURE", "FOOTER", "FORM", "H1", "H2", "H3", "H4", "H5", "H6", "HEADER", "HR", "I", "IFRAME", "IMG", "INPUT", "INS", "KBD", "LABEL", "LEGEND", "LI", "MAIN", "MARK", "METER", "NAV", "OBJECT", "OL", "OPTGROUP", "OPTION", "P", "PRE", "PROGRESS", "Q", "SAMP", "SECTION", "SELECT", "SMALL", "SOURCE", "SPAN", "STRONG", "SUB", "SUMMARY", "SUP", "TABLE", "TBODY", "TD", "TEXTAREA", "TFOOT", "TH", "THEAD", "TIME", "TR", "U", "UL", "VAR", "VIDEO"]
    let unsupported_elems = ["#comment"]

    if elem (nodeName n) normal_elems then
      handleNode clargs n (toLower $ nodeName n)
    else if elem (nodeName n) unsupported_elems then do
      addNoHalogenError "Found node \"s\", skipping."
      pure []
    else
      if cli_options.format `elem` [Raw, App] then do
        addConvertedToRawInfo $ "Node \"" <> (nodeName n) <> "\""
        handleRawNode n
      else do
        addUnimplementedError $ "Handling of node \"" <> (nodeName n) <> "\""
        pure []


-- excludes id and class? attribute (handled separately)
attributesToAST :: Partial => Node -> FO.Object String -> StateT RunState Effect (Array (Expr Void))
attributesToAST n attributes = FO.foldM (helper n) [] attributes

  where helper :: Partial => Node -> Array (Expr Void) -> String -> String -> StateT RunState Effect (Array (Expr Void))
        helper n acc k v = do
          ast <- attributeInfoAST n k v
          case ast of
            Nothing   -> pure acc
            Just expr -> pure $ acc <> [expr]

        attributeInfoAST :: Partial => Node -> String -> String -> StateT RunState Effect (Maybe (Expr Void))
        attributeInfoAST n k v =
          let isIntProp     = elem k $ ["cols", "rows", "colSpan", "rowSpan", "selectedIndex", "tabIndex"]
              isStringProp  = k `elem` ["alt", "charset", "for", "href", "name", "rel", "src", "style", "target", "title", "download", "action", "value", "placeholder", "list", "pattern", "poster"]
              isBooleanProp = k `elem` ["noValidate", "disabled", "enabled", "required", "readOnly", "spellcheck", "checked", "selected", "autocomplete", "autofocus", "multiple", "autoplay", "controls", "loop", "muted", "draggable"]
              isInputType   = (nodeName n) == "INPUT" && k == "type"
              inputTypes    = ["Button", "Checkbox", "Color", "Date", "DatetimeLocal", "Email", "File", "Hidden", "Image", "Month", "Number", "Password", "Radio", "Range", "Reset", "Search", "Submit", "Tel", "Text", "Time", "Url", "Week"]
              inputValue    = filter (\(Tuple halogenInputType htmlInputType) -> htmlInputType == v) $ zip inputTypes $ map toLower inputTypes
          in case k of
               key
                 | key == "id"   -> pure Nothing -- already have a separate implementation for handling the id elswhere
                 | isStringProp  ->
                     if (nodeName n) == "METER" then do
                       addNoHalogenError "Property \"value\" on tag \"meter\" is broken. https://github.com/purescript-halogen/purescript-halogen/issues/785"
                       pure Nothing
                     else
                       pure $ Just $ exprApp (exprIdent $ "HP." <> key) [ exprString v ]
                 | isIntProp     -> do
                   case fromString v of
                     Nothing -> do
                       addInvalidHTMLError $ "Found property \"" <> k <> "\" with value \"" <> v <> "\". This is supposed to be an integer."
                       pure Nothing
                     Just v_int -> pure $ Just $ exprApp (exprIdent $ "HP." <> key) [ exprInt v_int ]
                 | isBooleanProp -> pure $ Just $ exprApp (exprIdent $ "HP." <> key) [ exprBool true ] -- the fact the property was found makes it enabled
                 | isInputType   ->
                     case length inputValue of
                       0 -> do
                         addInvalidHTMLError $ "Input type was set in invalid value \"" <> v <> "\""
                         pure Nothing
                       1 -> pure $ Just $ exprApp (exprIdent $ "HP.type_") [ exprCtor $ "HP.Input" <> (fst $ unsafePartial $ fromJust $ head inputValue) ]
                       _ -> do
                         addError $ "error[program error] this branch should be impossible to reach. Found an input tag with multiple input values for a single type attribute."
                         pure Nothing
                 | otherwise     -> do
                     addUnimplementedError $ "Attribute \"" <> k <> "\" on \"" <> (nodeName n) <> "\""
                     pure Nothing


allAttributesToAST :: Partial => Node -> StateT RunState Effect (Array (Expr Void))
allAttributesToAST n = (liftEffect $ maybeShortElementInfo n) >>=
  case _ of
    Nothing -> pure []
    Just e_info -> do
      let (ElementId element_id) = e_info.id
          id = if String.null element_id then
                 []
               else
                 [ exprApp (exprIdent "HP.id") [exprString element_id] ]

      attributes <- attributesToAST n e_info.attributes

      classes <- do
        no_css_helper <- gets (_.noCssHelper)
        if no_css_helper then
          let first_part :: String -> Expr Void -> Expr Void
              first_part classname expr = exprOp (exprIdent classname) [ binaryOp "$" (exprApp (exprIdent "coerce") [expr] )]
          in pure $ case length (e_info.classList) of
               0 -> []
               1 -> [ first_part "HP.class_" $ exprString $ unsafePartial $ fromJust $ head e_info.classList ]
               _ -> [ first_part "HP.classes" $ exprArray $ map exprString e_info.classList ]
        else pure
          case length (e_info.classList) of
            0 -> []
            1 -> [ exprApp (exprIdent "css") [exprString $ unsafePartial $ fromJust $ head e_info.classList] ]
            _ -> [ exprApp (exprIdent "css") [exprString $ intercalate " " e_info.classList] ]

      when (not $ null classes) $ modify_ (_ { sawClasses = true })

      pure $ id <> attributes <> classes


handleRawNode :: Partial => Node -> StateT RunState Effect (Array (Expr Void))
handleRawNode n = case fromNode n of
  Nothing -> do
    addProgramError $ "Could not convert node \"" <> (nodeName n) <> "\" into Element."
    pure []
  Just e -> do
    let outer_html = elementGetOuterHtml e
    pure [ exprApp (exprIdent "Raw.raw") [exprString outer_html] ]


-- takes care of generating right halogen function name and the attributes for the element
-- children can be passed as desired
handleNodeAttributes :: Partial => Node -> String -> Array (Expr Void) -> StateT RunState Effect (Array (Expr Void))
handleNodeAttributes n function_name_with_props children = do
  let function_name_with_props2 = "HH." <> function_name_with_props
  let function_name_no_props = function_name_with_props2 <> "_"

  combined_attributes <- allAttributesToAST n

  if null combined_attributes then
    pure [ exprApp (exprIdent function_name_no_props) children ]
  else
    pure [ exprApp (exprIdent function_name_with_props2) ([exprArray combined_attributes] <> children) ]


handleNodeWithChildren :: Partial => Node -> String -> (Node -> StateT RunState Effect (Array (Expr Void))) -> StateT RunState Effect (Array (Expr Void))
handleNodeWithChildren n function_name_with_props makeChildren = do
  -- No children when only whitespace text
  children <- do
    only_ws <- liftEffect $ noChildrenAndOnlyWhitespace n
    if only_ws then
      pure []
    else
      makeChildren n

  let halogen_self_closing_tags = ["AREA", "BASE", "BR", "COL", "COMMAND", "HR", "IFRAME", "IMG", "INPUT", "LINK", "META", "PARAM", "SOURCE", "TEXTAREA", "TRACK", "WBR"]

  children2 <-
    if notElem (nodeName n) halogen_self_closing_tags then
      pure [ exprArray children ]
    else if null children then
      pure []
    else do
      addNoHalogenError $ "Node \"" <> (nodeName n) <> "\" had child nodes but halogen does not allow it."
      pure []

  handleNodeAttributes n function_name_with_props children2


handleNode :: Partial => CLArgs -> Node -> String -> StateT RunState Effect (Array (Expr Void))
handleNode clargs n function_name_with_props = handleNodeWithChildren n function_name_with_props $ \node -> do
  children_expr <- getChildrenExpr clargs node
  mergeChildren n children_expr


noChildrenAndOnlyWhitespace :: Partial => Node -> Effect Boolean
noChildrenAndOnlyWhitespace n = do
  text_content <- textContent n
  child_nodes <- liftEffect $ childNodes n >>= toArray
  let remaining_child_nodes = filter (\node -> nodeName node /= "#text") child_nodes
  pure $ (String.null (trim text_content)) && (null remaining_child_nodes)


main :: Effect Unit
main = do
  makeNodeCompatible
  clargs@(CLArgs cli_options) <- execParser cliOptions
  html <- readTextFile UTF8 cli_options.inputFilepath
  parser <- makeDOMParser
  doc <- parseHTMLFromString html parser
  case doc of
    Left err -> log "hello"
    Right doc -> do
      let n = toNode doc
      (Tuple codegen_module { errors }) <- runner clargs n
      let ps_code = printModule codegen_module
      let errors2 = nub $ sort errors -- a bit tougher to debug what happened in which order, but easier for the user
      for_ errors2 error
      writeTextFile UTF8 cli_options.outputFilepath ps_code
      pure unit

  log "🍝 lunch is ready"


runner :: CLArgs -> Node -> Effect (Tuple (Module Void) RunState)
runner clargs n = flip runStateT (initialRunState clargs) runCodegen
  where runCodegen :: StateT RunState Effect (Module Void)
        runCodegen = unsafePartial (halogenHeader clargs n)

halogenHeader :: CLArgs -> Node -> Partial => StateT RunState Effect (Module Void)
halogenHeader clargs@(CLArgs cli_options) n = do
  halogen_elements <- run clargs n

  let halogen_expression =
        if length halogen_elements == 1 then
          unsafePartial $ fromJust $ head halogen_elements
        else
          exprApp (exprIdent "HH.div_") [exprArray halogen_elements]

  saw_classes <- gets (_.sawClasses)

  let gen_imports =
        [ declImport "Prelude" [] ]
        <>
        ( if cli_options.format `elem` [File, App] then
            [ declImport "Effect" [ importType "Effect" ] ]
          else
            []
        )
        <>
        ( if cli_options.format `elem` [Raw, App] then
            [ declImport "Effect.Class" [ importClass "MonadEffect" ]
            , declImportAs "Halogen" [] "H"
            ]
          else
            []
        )
        <>
        ( if cli_options.format == App then
            [ declImportAs "Halogen.Aff" [] "HA" ]
          else
            []
        )
        <>
        [ declImportAs "Halogen.HTML" [] "HH"
        , declImportAs "Halogen.HTML.Properties" [] "HP" -- todo: this can be optional on whether an attribute was detected
        ]
        <>
        ( if cli_options.format `elem` [Raw, App] then
            [ declImportAs "Halogen.HTML.Raw" [] "Raw" ]
          else
            []
        )
        <>
        ( if cli_options.format == App then
            [ declImport "Halogen.VDom.Driver" [ importValue "runUI" ] ]
          else
            []
        )
        <>
        ( if cli_options.format == File then
            [ declImport "Halogen.VDom.DOM.StringRenderer" [ importValue "render" ]
            , declImport "Node.Encoding" [ importTypeMembers "Encoding" ["UTF8"] ]
            , declImport "Node.FS.Sync" [importValue "writeTextFile"]
            ]
          else
            []
        )
        <>
        ( if saw_classes && cli_options.noCssHelper then
            [declImport "Safe.Coerce" [importValue "coerce"]]
          else
            []
        )
        <>
        ( if cli_options.format `elem` [Raw, App] then
            [ declImport "Type.Row" [ importTypeOp "+" ]  ]
          else
            []
        )

  let gen_declarations =
        ( case cli_options.format of
            Plain ->
              [ declSignature "template" $ typeForall [typeVar "w", typeVar "i"] $ typeApp (typeCtor "HH.HTML") [typeVar "w", typeVar "i"]
              , declValue "template" [] halogen_expression
              ]
            File  ->
              [ declSignature "renderHtml" $ typeArrow [ typeApp (typeCtor "HH.HTML") [typeCtor "String", typeCtor "Void"] ] ( typeCtor "String" )
              , declValue "renderHtml" [binderParens $ binderCtor "HH.HTML" [binderVar "inner"]] $
                  exprApp (exprIdent "render") [exprIdent "identity", exprIdent "inner"]
              ]
            _     -> []
        )
        <>
        ( case cli_options.format of
            File ->
              [ declSignature "main" $ typeApp (typeCtor "Effect") [ typeCtor "Unit" ]
              , declValue "main" [] $
                  let first_part = exprApp (exprIdent "writeTextFile") [exprCtor "UTF8", exprString "output.html"]
                  in exprOp first_part [ binaryOp "$" (exprIdent "renderHtml"), binaryOp "$" halogen_expression ]
              ]
            App ->
              [ declSignature "main" $ typeApp (typeCtor "Effect") [ typeCtor "Unit" ]
              , declValue "main" [] (exprApp (exprIdent "HA.runHalogenAff")
                [ exprDo
                  [ doBind (binderVar "body") (exprIdent "HA.awaitBody")]
                  ( exprApp (exprIdent "runUI") (map exprIdent ["component", "unit", "body"]) )
                ])

              , declSignature "component" $
                 typeForall [typeVar "q", typeVar "i", typeVar "o", typeVar "m"] $
                   typeConstrained [typeApp (typeCtor "MonadEffect") [ typeVar "m" ]] $
                     typeApp (typeCtor "H.Component") [typeVar "q", typeVar "i", typeVar "o", typeVar "m"]
              , declValue "component" [] $ exprApp (exprIdent "H.mkComponent") $ [exprRecord
                [ Tuple "initialState" $ exprLambda [binderWildcard] (exprIdent "unit")
                , Tuple "render" $ exprLambda [binderWildcard] (exprIdent "template")
                , Tuple "eval" $ exprOp (exprIdent "H.mkEval")
                  [ binaryOp "$" $ exprUpdate (exprIdent "H.defaultEval") [update "handleAction" $ exprLambda [binderWildcard] (exprApp (exprIdent "pure") [exprIdent "unit"])]
                  ]
                ]]
              ]
            _ -> []
        )
        <>
        ( if cli_options.noCssHelper then
            []
          else
            [ declSignature "css" $
                typeForall [ typeVar "r", typeVar "i" ] $
                  typeArrow [typeCtor "String"]
                    (typeApp (typeCtor "HH.IProp") [typeRow [Tuple "class" (typeCtor "String")] (Just (typeVar "r")), typeVar "i"])
            , declValue "css" [] $
                exprOp (exprIdent "HP.class_") [ binaryOp "<<<" (exprCtor "HH.ClassName") ]
            ]
        ) <>
        ( if cli_options.format `notElem` [Plain, File] then
            [ leading (lineComments "H.ComponentHTML action slots m ~ HH.HTML (H.ComponentSlot slots m action) action") $
              declSignature "template" $ typeForall [typeVar "action", typeVar "m"] $
                typeConstrained [typeApp (typeCtor "MonadEffect") [ typeVar "m" ]] $
                  typeApp (typeCtor "H.ComponentHTML")
                    [ typeVar "action"
                    , typeOp (typeApp (typeCtor "Raw.Slot") [typeCtor "Unit"]) [binaryOp "+" typeRowEmpty]
                    , typeVar "m"
                    ]
            , declValue "template" [] halogen_expression
            ]
          else
            []
        )

  pure $ module_ "Main" [] gen_imports gen_declarations




-- Functions below are _mostly_ for debugging .. but some of them are also used by the code above

type NodeInfo =
  { childNodes :: NodeList
  , firstChild :: Maybe Node
  , hasChildNodes :: Boolean
  , lastChild :: Maybe Node
  , nextSibling :: Maybe Node
  , nodeName :: String
  , nodeType :: NodeType
  , nodeTypeIndex :: Int
  , nodeValue :: Maybe String
  , normalize :: Unit
  , ownerDocument :: Maybe Document
  , parentElement :: Maybe Element
  , parentNode :: Maybe Node
  , previousSibling :: Maybe Node
  , textContent :: String
  , element :: Maybe ElementInfo
  }

getNodeInfo :: Node -> Effect NodeInfo
getNodeInfo n = do
  let n_nodeType      = unsafePartial $ nodeType n
  let n_nodeTypeIndex = nodeTypeIndex n
  let n_nodeName      = nodeName n
  n_ownerDocument     <- ownerDocument n
  n_parentNode        <- parentNode n
  n_parentElement     <- parentElement n
  n_hasChildNodes     <- hasChildNodes n
  n_childNodes        <- childNodes n
  n_firstChild        <- firstChild n
  n_lastChild         <- lastChild n
  n_previousSibling   <- previousSibling n
  n_nextSibling       <- nextSibling n
  n_nodeValue         <- nodeValue n
  n_textContent       <- textContent n
  n_normalize         <- normalize n

  element_info <- case fromNode n of
    Nothing -> pure Nothing
    Just e -> Just <$> getElementInfo e

  pure
    { nodeType: n_nodeType
    , nodeTypeIndex: n_nodeTypeIndex
    , nodeName: n_nodeName
    , ownerDocument: n_ownerDocument
    , parentNode: n_parentNode
    , parentElement: n_parentElement
    , hasChildNodes: n_hasChildNodes
    , childNodes: n_childNodes
    , firstChild: n_firstChild
    , lastChild: n_lastChild
    , previousSibling: n_previousSibling
    , nextSibling: n_nextSibling
    , nodeValue: n_nodeValue
    , textContent: n_textContent
    , normalize: n_normalize
    , element: element_info
    }


type ShortNodeInfo =
  { childNodes :: Array Node
  , nodeName :: String
  , nodeType :: NodeType
  , nodeValue :: Maybe String
  , textContent :: String
  , element :: Maybe ShortElementInfo
  }

getShortNodeInfo :: Node -> Effect ShortNodeInfo
getShortNodeInfo n = do
  let n_nodeType = unsafePartial $ nodeType n
  let n_nodeName = nodeName n
  n_childNodes   <- childNodes n >>= toArray
  n_nodeValue    <- nodeValue n
  n_textContent  <- textContent n

  element_info <- case fromNode n of
    Nothing -> pure Nothing
    Just e -> Just <$> getShortElementInfo e

  pure
    { nodeType: n_nodeType
    , nodeName: n_nodeName
    , childNodes: n_childNodes
    , nodeValue: n_nodeValue
    , textContent: n_textContent
    , element: element_info
    }


type ElementInfo =
  { attributes :: Array Attr
  , classList :: DOMTokenList
  , className :: ClassName
  , id :: ElementId
  , localName :: ElementName
  , namespaceURI :: Maybe NamespaceURI
  , prefix :: Maybe NamespacePrefix
  , tagName :: ElementName
  }

getElementInfo :: Element -> Effect ElementInfo
getElementInfo e = do
  n_attributes <- attributes e >>= getAttributes
  let n_namespaceURI = namespaceURI e
  let n_prefix       = prefix e
  let n_localName    = localName e
  let n_tagName      = tagName e
  n_id        <- id e
  n_className <- className e
  n_classList <- classList e
  pure
    { attributes: n_attributes
    , namespaceURI: n_namespaceURI
    , prefix: n_prefix
    , localName: n_localName
    , tagName: n_tagName
    , id: n_id
    , className: n_className
    , classList: n_classList
    }


type ShortElementInfo =
  { attributes :: FO.Object String
  , classList :: Array String
  , id :: ElementId
  }


maybeShortElementInfo :: Node -> Effect (Maybe ShortElementInfo)
maybeShortElementInfo n = case fromNode n of
  Nothing -> pure Nothing
  Just e -> Just <$> getShortElementInfo e


getShortElementInfo :: Element -> Effect ShortElementInfo
getShortElementInfo e = do
  n_attributes <- nodeAttributesToFO e
  let n_attributes_no_class = FO.filterKeys (\key -> key /= "class") n_attributes
  n_id        <- id e
  n_classList <- classList e >>= tokens
  pure
    { attributes: n_attributes_no_class
    , id: n_id
    , classList: n_classList
    }
  where nodeAttributesToFO :: Element -> Effect (FO.Object String)
        nodeAttributesToFO e = map FO.fromFoldable $ attributes e >>= getAttributes >>= (traverse getShortAttrInfo)


type AttrInfo =
  { value :: String
  , localName :: String
  , namespaceURI :: Maybe NamespaceURI
  , prefix :: Maybe NamespacePrefix
  }

getAttrInfo :: Attr -> Effect AttrInfo
getAttrInfo a = do
  let namespaceURI = Attr.namespaceURI a
  let prefix = Attr.prefix a
  let (AttrName localName) = Attr.localName a
  value <- Attr.getValue a
  pure
    { namespaceURI: namespaceURI
    , prefix: prefix
    , localName: localName
    , value: value
    }

getShortAttrInfo :: Attr -> Effect (Tuple String String)
getShortAttrInfo a = do
  let (AttrName localName) = Attr.localName a
  value <- Attr.getValue a
  pure $ Tuple localName value

module Main where

import Prelude

import Control.Monad.State
import Data.Array hiding (fold, length, null, foldM, elem, notElem)
import Data.Either
import Data.Foldable
import Data.Generic.Rep (class Generic)
import Data.Maybe
import Data.Show.Generic (genericShow)
import Data.Traversable
import Data.Tuple
import Debug
import Effect (Effect)
import Effect.Class
import Effect.Console (log, error)
import Foreign.Object as FO
import Node.Encoding
import Node.FS.Sync
import Options.Applicative
import Partial.Unsafe
import PureScript.CST.Types
import Tidy.Codegen
import Web.DOM.Attr (Attr)
import Web.DOM.Attr as Attr
import Web.DOM.Document hiding (fromNode)
import Web.DOM.DOMParser
import Web.DOM.DOMTokenList
import Web.DOM.Element hiding (toNode)
import Web.DOM.NamedNodeMap
import Web.DOM.Node
import Web.DOM.NodeList hiding (length)
import Web.DOM.NodeType
import PureScript.CST.Print
import Data.String hiding (null, length)
import Data.String as String
import Data.Int


foreign import makeNodeCompatible :: Effect Unit

data CLArgs = CLArgs
  { inputFilepath :: String
  , outputFilepath :: String
  }

derive instance genericCLArgs :: Generic CLArgs _

instance showCLArgs :: Show CLArgs where
  show = genericShow

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

  in CLArgs { inputFilepath, outputFilepath }

cliOptions = info (cl_args_parser <**> helper)
  ( fullDesc
 <> header "html2halogen" )

type RunState =
  { errors :: Array String
  , waitForBody :: Boolean -- "</head> <body>" will yield one single space #text element, which we want to ignore. This flag helps
  , sawClasses :: Boolean -- the test html doesn't actually use classes so we have an unsed import Unsafe.Coerce
  }
intialRunState =
  { errors: []
  , waitForBody: false
  , sawClasses: false
  }


getChildrenExpr :: Partial => Node -> StateT RunState Effect (Array (Array (Expr Void)))
getChildrenExpr n = do
  child_nodes <- liftEffect $ childNodes n >>= toArray
  traverse run child_nodes

addInvalidHTMLError :: String -> StateT RunState Effect Unit
addInvalidHTMLError err = addError $ "error[invalid HTML] " <> err

addUnimplementedError :: String -> StateT RunState Effect Unit
addUnimplementedError err = addError $ "error[unimplemented] " <> err

addNoHalogenError :: String -> StateT RunState Effect Unit
addNoHalogenError err = addError $ "error[no halogen equivalent] " <> err

addError :: String -> StateT RunState Effect Unit
addError err = modify_ \rec@{ errors } -> rec { errors = errors <> [err] }

addContinueError :: String -> StateT RunState Effect Unit
addContinueError node_name = addNoHalogenError $ "Found node \"" <> node_name <> "\", continuing with it's child nodes."


n_html :: Partial => Node -> StateT RunState Effect (Array (Expr Void))
n_html n = do
  modify_ (_ { waitForBody = true })
  (liftEffect $ hasChildNodes n) >>= case _ of
    false -> do
      addNoHalogenError "Found node \"html\" with no children, this is probably the doctype."
      pure []
    true  -> errorAndContinue n


errorAndContinue :: Partial => Node -> StateT RunState Effect (Array (Expr Void))
errorAndContinue n = do
  addContinueError $ nodeName n
  children_expr <- getChildrenExpr n

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
              addError $ "error[program error] found a child that returned multiple elements in parent \"" <> (nodeName n) <> "\""
              pure acc




-- liftEffect $ getShortNodeInfo n >>= traceM
run :: Partial => Node -> StateT RunState Effect (Array (Expr Void))
run n = case nodeName n of
  "#document" -> do
    modify_ (_ { waitForBody = true })
    errorAndContinue n
  "html" -> n_html n
  "HTML" -> n_html n
  "HEAD" -> do
    modify_ (_ { waitForBody = true })
    addNoHalogenError "Found node \"head\", skipping."
    pure []
  "BODY" -> do
    modify_ (_ { waitForBody = false })
    errorAndContinue n
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
      handleNode n "canvas"
    else do
      addUnimplementedError "Handling of node \"canvas\" when it has children."
      pure []
  -- "BR" -> do
  --     addUnimplementedError "Handling of node \"br\"."
  --     pure []
  -- "HR" -> do
  --     addUnimplementedError "Handling of node \"br\"."
  --     pure []
  _ -> do
    let normal_elems = ["A", "ABBR", "ADDRESS", "ARTICLE", "AUDIO", "B", "BLOCKQUOTE", "BR", "BUTTON", "CAPTION", "circle", "CITE", "CODE", "DATALIST", "DD", "DEL", "DETAILS", "DFN", "DIV", "DL", "DT", "EM", "EMBED", "FIELDSET", "FIGCAPTION", "FIGURE", "FOOTER", "FORM", "H1", "H2", "H3", "H4", "H5", "H6", "HEADER", "HR", "I", "IFRAME", "IMG", "INPUT", "INS", "KBD", "LABEL", "LEGEND", "LI", "MAIN", "MARK", "METER", "NAV", "OBJECT", "OL", "OPTGROUP", "OPTION", "P", "PRE", "PROGRESS", "Q", "SAMP", "SECTION", "SELECT", "SMALL", "SOURCE", "SPAN", "STRONG", "SUB", "SUMMARY", "SUP", "TABLE", "TBODY", "TD", "TEXTAREA", "TFOOT", "TH", "THEAD", "TIME", "TR", "U", "UL", "VAR", "VIDEO"]
    let unsupported_elems = ["#comment", "PICTURE", "S", "svg"]

    if elem (nodeName n) normal_elems then
      handleNode n (toLower $ nodeName n)
    else if elem (nodeName n) unsupported_elems then do
      addNoHalogenError "Found node \"s\", skipping."
      pure []
    else do
      addUnimplementedError $ "Handling of node \"" <> (nodeName n) <> "\""
      -- node_info <- liftEffect $ getShortNodeInfo n
      pure []


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


handleNode :: Partial => Node -> String -> StateT RunState Effect (Array (Expr Void))
handleNode n function_name_with_props = handleNodeWithChildren n function_name_with_props $ \node -> do
  children_expr <- getChildrenExpr node
  mergeChildren n children_expr


handleNodeWithChildren :: Partial => Node -> String -> (Node -> StateT RunState Effect (Array (Expr Void))) -> StateT RunState Effect (Array (Expr Void))
handleNodeWithChildren n function_name_with_props makeChildren = do
  -- liftEffect $ getShortNodeInfo n >>= traceM
  let function_name_with_props2 = "HH." <> function_name_with_props
  let function_name_no_props = function_name_with_props2 <> "_"

  combined_attributes <- (liftEffect $ maybeShortElementInfo n) >>=
    case _ of
      Nothing -> pure []
      Just e_info -> do
        let id = if String.null e_info.id then
                   []
                 else
                   [ exprApp (exprIdent "HP.id") [exprString e_info.id] ]

        attributes <- attributesToAST n e_info.attributes

        let classes =
              let first_part :: String -> Expr Void -> Expr Void
                  first_part classname expr = exprOp (exprIdent classname) [ binaryOp "$" (exprApp (exprIdent "coerce") [expr] )]
              in case length (e_info.classList) of
                   0 -> []
                   1 -> [ first_part "HP.class_" $ exprString $ unsafePartial $ fromJust $ head e_info.classList ]
                   _ -> [ first_part "HP.classes" $ exprArray $ map exprString e_info.classList ]

        when (not $ null classes) $ modify_ (_ { sawClasses = true })

        pure $ id <> attributes <> classes

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

  if null combined_attributes then
    pure [ exprApp (exprIdent function_name_no_props) children2 ]
  else
    pure [ exprApp (exprIdent function_name_with_props2) ([exprArray combined_attributes] <> children2) ]


noChildrenAndOnlyWhitespace :: Partial => Node -> Effect Boolean
noChildrenAndOnlyWhitespace n = do
  text_content <- textContent n
  child_nodes <- liftEffect $ childNodes n >>= toArray
  let remaining_child_nodes = filter (\node -> nodeName node /= "#text") child_nodes
  pure $ (String.null (trim text_content)) && (null remaining_child_nodes)


main :: Effect Unit
main = do
  makeNodeCompatible
  (CLArgs cli_options) <- execParser cliOptions
  html <- readTextFile UTF8 cli_options.inputFilepath
  parser <- makeDOMParser
  doc <- parseHTMLFromString html parser
  case doc of
    Left err -> log "hello"
    Right doc -> do
      let n = toNode doc
      (Tuple codegen_module { errors }) <- runner n
      let ps_code = printModule codegen_module
      let errors2 = nub $ sort errors -- a bit tougher to debug what happened in which order, but easier for the user
      for_ errors2 error
      writeTextFile UTF8 cli_options.outputFilepath ps_code
      pure unit

  log "🍝 lunch is ready"


runner :: Node -> Effect (Tuple (Module Void) RunState)
runner n = flip runStateT intialRunState runCodegen
  where runCodegen :: StateT RunState Effect (Module Void)
        runCodegen = unsafePartial (halogenHeader n)

halogenHeader :: Node -> Partial => StateT RunState Effect (Module Void)
halogenHeader n = do
  halogen_elements <- run n

  let halogen_expression =
        if length halogen_elements == 1 then
          unsafePartial $ fromJust $ head halogen_elements
        else
          exprApp (exprIdent "HH.div_") [exprArray halogen_elements]

  saw_classes <- gets (_.sawClasses)

  pure $ module_ "Main" []
    (
      [ declImport "Prelude" []
      , declImport "Effect" [ importType "Effect" ]
      , declImportAs "Halogen.HTML" [] "HH"
      , declImportAs "Halogen.HTML.Properties" [] "HP"
      , declImport "Halogen.VDom.DOM.StringRenderer" [ importValue "render" ]
      , declImport "Node.Encoding" [ importTypeMembers "Encoding" ["UTF8"] ]
      , declImport "Node.FS.Sync" [importValue "writeTextFile"]
      ]
    <> (if saw_classes then [declImport "Safe.Coerce" [importValue "coerce"]] else [])
    )
    [ declSignature "renderHtml" $ typeArrow [ typeApp (typeCtor "HH.HTML") [typeCtor "String", typeCtor "Void"] ] ( typeCtor "String" )
    , declValue "renderHtml" [binderParens $ binderCtor "HH.HTML" [binderVar "inner"]] $
        exprApp (exprIdent "render") [exprIdent "identity", exprIdent "inner"]

    , declSignature "main" $ typeApp (typeCtor "Effect") [ typeCtor "Unit" ]
    , declValue "main" [] $
        let first_part = exprApp (exprIdent "writeTextFile") [exprCtor "UTF8", exprString "output.html"]
        in exprOp first_part [ binaryOp "$" (exprIdent "renderHtml"), binaryOp "$" halogen_expression ]
    ]



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
  , className :: String
  , id :: String
  , localName :: String
  , namespaceURI :: Maybe String
  , prefix :: Maybe String
  , tagName :: String
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
  , id :: String
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
  , name :: String
  , namespaceURI :: String
  , prefix :: String
  }

getAttrInfo :: Attr -> Effect AttrInfo
getAttrInfo a = do
  namespaceURI <- Attr.namespaceURI a
  prefix       <- Attr.prefix a
  localName    <- Attr.localName a
  name         <- Attr.name a
  value        <- Attr.getValue a
  pure
    { namespaceURI: namespaceURI
    , prefix: prefix
    , localName: localName
    , name: name
    , value: value
    }

getShortAttrInfo :: Attr -> Effect (Tuple String String)
getShortAttrInfo a = do
  name  <- Attr.name a
  value <- Attr.getValue a
  pure $ Tuple name value

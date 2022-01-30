# html2halogen

## running app
```shell
spago run -a '--help'
```

## Development

### automatic rebuild for development

#### deno
```shell
spago bundle-app --then "sed -i.old '1s;^;#\!/usr/bin/env -S deno run --compat --unstable --allow-net --allow-read\n\n;' index.js" --watch
```
https://deno.land/manual@v1.16.3/npm_nodejs/compatibility_mode

#### node
```shell
spago bundle-app --then "sed -i.old '1s;^;#\!/usr/bin/env node\n\n;' index.js" --watch
```

##### possible alternative to sed
```shell
printf '%s\n' H '1g/^#!/q' 1i '#!/usr/bin/env node' . w q | ed -s filename
```

##### changing terminal title within zsh
```shell
spago bundle-app --before " zsh -c \"export DISABLE_AUTO_TITLE=true; print -Pn \\\"\e]2;spago compiling …\a\\\"\"" --then "sed -i.old '1s;^;#\!/usr/bin/env node\n\n;' index.js; zsh -c \"print -Pn \\\"\e]2;spago ready\a\\\"\"" --else "zsh -c \"print -Pn \\\"\e]2;spago failed\a\\\"\"" --watch
```

### testing app
https://github.com/flip111/html-differ/tree/patch-1

```shell
./index.js --input test.html --output 'generated/src/Main.purs'  && (cd generated; spago run; cd ..) && html-differ test.html generated/output.html
```

### todo
1. make a split between app code and library code. The main part of the code runs on DOMParser so could run in the browser too
2. consider integration with https://github.com/purescript-halogen/purescript-halogen-css
3. Add support for properties
  - min, max -> Number (float)
  - step -> StepValue
  - height, width -> CSSPixel (just an int)
  - scope -> scopeValue
  - method -> FormMethod
  - enctype -> MediaType
  - accept -> InputAcceptType
  - preload -> PreloadValue
  - type_ -> ??
  - height, width -> CSSPixel .. have to investigate if HTML only allows 'px' here
4. Fix stuff coming up from test.html (see below)

#### program output on test.html
```
error[invalid HTML] Input type was set in invalid value "datetime"
error[invalid HTML] Input type was set in invalid value "datetime-local"
error[no halogen equivalent] Found node "#document", continuing with it's child nodes.
error[no halogen equivalent] Found node "BODY", continuing with it's child nodes.
error[no halogen equivalent] Found node "HTML", continuing with it's child nodes.
error[no halogen equivalent] Found node "head", skipping.
error[no halogen equivalent] Found node "html" with no children, this is probably the doctype.
error[no halogen equivalent] Found node "s", skipping.
error[no halogen equivalent] Property "value" on tag "meter" is broken. https://github.com/purescript-halogen/purescript-halogen/issues/785
error[unimplemented] Attribute "cite" on "Q"
error[unimplemented] Attribute "data" on "OBJECT"
error[unimplemented] Attribute "datetime" on "TIME"
error[unimplemented] Attribute "height" on "EMBED"
error[unimplemented] Attribute "height" on "IFRAME"
error[unimplemented] Attribute "height" on "OBJECT"
error[unimplemented] Attribute "label" on "OPTGROUP"
error[unimplemented] Attribute "max" on "INPUT"
error[unimplemented] Attribute "max" on "METER"
error[unimplemented] Attribute "min" on "INPUT"
error[unimplemented] Attribute "min" on "METER"
error[unimplemented] Attribute "role" on "DIV"
error[unimplemented] Attribute "type" on "BUTTON"
error[unimplemented] Attribute "type" on "OL"
error[unimplemented] Handling of node "canvas" when it has children.
```

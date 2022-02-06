"use strict";

exports.makeNodeCompatible = function() {
  // https://stackoverflow.com/a/55668667/1833322
  if (typeof(DOMParser) === 'undefined') { // if DOMParser is not defined then assign jsdom DOMParser();
    (function(global) {
      var jsdom = require('jsdom'),
          dom = new jsdom.JSDOM('');

      global.DOMParser = dom.window.DOMParser;
    })(global);
  }
};

// Could use this if node detection is needed:
//
// if(typeof process === 'object' && process + '' === '[object process]'){
//     // is node
// }
// else{
//     // not node
// }

exports.elementGetOuterHtml = function(element) {
  return element.outerHTML;
};
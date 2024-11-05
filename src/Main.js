"use strict";

export function makeNodeCompatible() {
  if (typeof window === "undefined") {
    import('jsdom').then(jsdom => {
      const { JSDOM } = jsdom;
      const dom = new JSDOM('');
      globalThis.DOMParser = dom.window.DOMParser;
    }).catch(error => {
      console.error("Error loading jsdom:", error);
    });
  }
}

export function elementGetOuterHtml(element) {
  return element.outerHTML;
};
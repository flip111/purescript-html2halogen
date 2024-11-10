"use strict";

export function makeNodeCompatible() {
  if (typeof window === "undefined") {
    return import('jsdom').then(jsdom => {
      const { JSDOM } = jsdom;
      const dom = new JSDOM('');
      globalThis.DOMParser = dom.window.DOMParser;
    }).catch(error => {
      console.error("Error loading jsdom:", error);
    });
  } else {
    return Promise.resolve(); // Dummy Promise that always resolves in browser environment
  }
}

export function elementGetOuterHtml(element) {
  return element.outerHTML;
};

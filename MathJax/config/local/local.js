MathJax.Hub.Config({
  TeX: {
    Macros: {
      fc: ["{\\frac{#1}{#2}}", 2],
      R: "{\\mathbb{R}}"
    }
  }
});

MathJax.Ajax.loadComplete("[MathJax]/config/local/local.js");

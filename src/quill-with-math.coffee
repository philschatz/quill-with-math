Quill = require 'quill'
MathTooltipFn = require './math-tooltip'
MONKEYPATCH = require './monkeypatch' # Apply patches so copy/paste works and so `math` is one of the formats


# Provide a version of katex to use so we do not include multiple versions
module.exports = (katex) ->

  MathTooltip = MathTooltipFn(katex)

  Quill.registerModule('math-tooltip', MathTooltip)
  return Quill

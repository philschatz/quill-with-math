katex   = require('katex')

Quill = require('./quill-with-math.coffee')(katex)

module.exports = Quill
window.Quill = Quill

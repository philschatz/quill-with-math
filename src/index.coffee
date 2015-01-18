katex   = require('katex')

Quill = require('./quill-with-math')(katex)

module.exports = Quill
window.Quill = Quill

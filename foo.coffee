Quill = require 'quill'
MathTooltip = require './src/math-tooltip.coffee'


editor = new Quill('#editor')
editor.addModule('toolbar', { container: '#toolbar' })
editor.addModule('link-tooltip', true)
editor.addModule('math-tooltip', true)



# editor.addFormat 'math',
#   attribute: 'data-math'
#   tag: 'SPAN'








window.EDITOR = editor
window.Quill = Quill

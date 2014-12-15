Quill   = require('quill')
Tooltip = Quill.modules.tooltip # require('./tooltip.coffee')
_       = Quill.require('lodash')
dom     = Quill.require('dom')

katex   = require('katex')

class MathTooltip extends Tooltip
  @DEFAULTS:
    maxLength: 50
    template:
     '<span class="title">Edit &nbsp;</span>
      <input class="math-input" type="text">
      <span>&nbsp;&#45;&nbsp;</span>
      <div class="preview"></div>
      <div class="preview-error"></div>
      <button class="cancel">Cancel</button>
      <button class="done">Done</button>'

  constructor: (@quill, @options) ->
    @options = _.defaults(@options, Tooltip.DEFAULTS)
    super(@quill, @options)
    dom(@container).addClass('ql-math-tooltip')
    @textbox = @container.querySelector('.math-input')
    @preview = @container.querySelector('.preview')
    @previewError = @container.querySelector('.preview-error')
    @initListeners()

  initListeners: ->
    @quill.on(@quill.constructor.events.SELECTION_CHANGE, (range) =>
      renderAllMath(@quill)
      return unless range? and range.isCollapsed()
      anchor = @_findAnchor(range)
      if anchor
        # @setMode(dom(anchor).attributes()['data-math'].substring('math:'.length), false)

        # @quill.setSelection(start, end)

        @setMode(dom(anchor).attributes()['data-math'].substring('math:'.length), true)
        @show(anchor)
      else
        @range = null # Tooltip.hide will try to use this which causes the tooltip to open back up
        @range = null   # Prevent restoring selection to last saved
        @hide()
    )
    dom(@container.querySelector('.done')).on('click', _.bind(@saveMath, @))
    dom(@container.querySelector('.cancel')).on('click', _.bind(@hide, @))
    # dom(@container.querySelector('.change')).on('click', =>
    #   @setMode(dom(@link).attributes()['data-math'].substring('#'.length), true)
    @range = null # Tooltip.hide will try to use this which causes the tooltip to open back up
    # )

    # initTextbox: (textbox, enterCallback, escapeCallback)
    @initTextbox(@textbox, @saveMath, @hide)
    @_updateMathPreview()
    @quill.onModuleLoad('toolbar', (toolbar) =>
      toolbar.initFormat('math', _.bind(@_onToolbar, @))
    )

    dom(@textbox).on('keyup', _.bind(@_updateMathPreview, @))

    renderAllMath(@quill)


  _updateMathPreview: ->
    if @currentMathEl
      originalFormula = dom(@currentMathEl).attributes()['data-math'].substring('math:'.length)
      formula = @textbox.value
      if formula isnt originalFormula
        try
          katex.render(formula, @preview)
          @previewError.innerHTML = ''
        catch e
          @previewError.innerHTML = "Parse Problem: #{e.message}"
      else
        @preview.innerHTML = ''
        @previewError.innerHTML = ''


  renderMath: (node) ->
    formula = dom(node).attributes()['data-math'].substring('math:'.length)
    try
      katex.render(formula, node)
    catch e
      node.innerHTML = e.message


  show: (@currentMathEl) ->
    super(arguments...)
    # @initialFormula = dom(@currentMathEl).attributes()['data-math'].substring('math:'.length)

  hide: ->
    @range = null # Tooltip.hide will try to use this which causes the tooltip to open back up
    super(arguments...)


  saveMath: ->
    url = @textbox.value
    if @range?
      if @range.isCollapsed()
        anchor = @_findAnchor(@range)
        dom(anchor).attributes({'data-math':"math:#{url}"}) if anchor?
        @renderMath(anchor)
      else
        @quill.formatText(@range, 'math', url, 'user')
        # HACK: Render all Math. Should only render new Math elements (:not(.rendered))
        for math in @quill.editor.root.querySelectorAll('[data-math^="math:"]')
          formula = dom(math).attributes()['data-math'].substring('math:'.length)
          try
            katex.render(formula, math)
          catch e
            console.log 'Error: Invalid math'

    @setMode(url, false)
    @hide()

  setMode: (url, edit = false) ->
    if edit
      @textbox.value = url
      _.defer( =>
        # Setting value and immediately focusing doesn't work on Chrome
        @textbox.focus()
        @textbox.setSelectionRange(url.length, url.length)
      )
    else
      @textbox.value = url
      # @link.data-math = url
      # text = if url.length > @options.maxLength then url.slice(0, @options.maxLength) + '...' else url
      # dom(@link).text(text)
    dom(@container).toggleClass('editing', edit)

  _findAnchor: (range) ->
    [leaf, offset] = @quill.editor.doc.findLeafAt(range.start, true)
    node = leaf.node if leaf?
    while node?
      return node if dom(node).attributes()['data-math']?.substring('math:'.length)
      node = node.parentNode
    return null

  _onToolbar: (range, value) ->
    return unless range and !range.isCollapsed()
    if value
      @setMode(@_suggestURL(range), true)
      nativeRange = @quill.editor.selection._getNativeRange()
      @show(nativeRange)
    # else
    #   @quill.formatText(range, 'link', false, 'user')

  _suggestURL: (range) ->
    text = @quill.getText(range)
    return text # @_normalizeURL(text)


renderAllMath = (quill) ->
  # Render math
  for math in quill.editor.root.querySelectorAll('[data-math^="math:"]:not(.loaded)')
    # math.contentEditable = false
    formula = dom(math).attributes()['data-math'].substring('math:'.length)
    try
      katex.render(formula, math)
      math.classList.add('loaded')
    catch e
      console.log 'Error: Invalid math'


Quill.registerModule('math-tooltip', MathTooltip)
module.exports = MathTooltip

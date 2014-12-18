(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var Format, Leaf, Leaf_isLeafNode, Line, Line_findLeaf, MathTooltip, Normalizer, Normalizer_optimizeLine, Normalizer_whitelistStyles, Quill, dom, editor, katex, matches, _;

Quill = require('quill');

MathTooltip = require('./src/math-tooltip.coffee');

_ = Quill.require('lodash');

dom = Quill.require('dom');

Normalizer = Quill.require('normalizer');

Line = Quill.require('core/line');

Leaf = Quill.require('core/leaf');

Format = Quill.require('core/format');

katex = require('katex');

Quill.DEFAULTS.formats.push('math');

Format.FORMATS['math'] = {
  attribute: 'data-math',
  tag: 'SPAN'
};

editor = new Quill('#editor');

editor.addModule('toolbar', {
  container: '#toolbar'
});

editor.addModule('link-tooltip', true);

editor.addModule('math-tooltip', true);

matches = function(el, selector) {
  var m;
  m = el.matches || el.matchesSelector || el.msMatchesSelector || el.mozMatchesSelector || el.webkitMatchesSelector || el.oMatchesSelector;
  if (!m) {
    return false;
  }
  return m.call(el, selector);
};

Normalizer_whitelistStyles = Normalizer.whitelistStyles;

Normalizer.whitelistStyles = function(node) {
  if (matches(node, '[data-math^="math:"] *')) {
    return;
  }
  return Normalizer_whitelistStyles(node);
};

Normalizer_optimizeLine = Normalizer.optimizeLine;

Normalizer.optimizeLine = function(lineNode) {
  var lineNodeLength, node, nodes, _results;
  lineNode.normalize();
  lineNodeLength = dom(lineNode).length();
  nodes = dom(lineNode).descendants();
  _results = [];
  while (nodes.length > 0) {
    node = nodes.pop();
    if ((node != null ? node.parentNode : void 0) == null) {
      continue;
    }
    if (dom.EMBED_TAGS[node.tagName] != null) {
      continue;
    }
    if (node.tagName === dom.DEFAULT_BREAK_TAG) {
      if (lineNodeLength !== 0) {
        _results.push(dom(node).remove());
      } else {
        _results.push(void 0);
      }
    } else if (dom(node).length() === 0 && !matches(node, '[data-math^="math:"] *')) {
      nodes.push(node.nextSibling);
      _results.push(dom(node).unwrap());
    } else if ((node.previousSibling != null) && node.tagName === node.previousSibling.tagName) {
      if (_.isEqual(dom(node).attributes(), dom(node.previousSibling).attributes())) {
        nodes.push(node.firstChild);
        _results.push(dom(node.previousSibling).merge(node));
      } else {
        _results.push(void 0);
      }
    } else {
      _results.push(void 0);
    }
  }
  return _results;
};

Line_findLeaf = Line.prototype.findLeaf;

Line.prototype.findLeaf = function(leafNode) {
  var curLeaf, found, leafNodeDom;
  found = Line_findLeaf.apply(this, arguments);
  if (found != null) {
    return found;
  } else {
    leafNodeDom = dom(leafNode);
    curLeaf = this.leaves.first;
    while (curLeaf != null) {
      if (leafNodeDom.isAncestor(curLeaf.node)) {
        return curLeaf;
      }
      curLeaf = curLeaf.next;
    }
  }
};

Leaf_isLeafNode = Leaf.isLeafNode;

Leaf.isLeafNode = function(node, formats) {
  if (matches(node, '[data-math^="math:"] .katex')) {
    return true;
  }
  return Leaf_isLeafNode.apply(this, arguments);
};

window.EDITOR = editor;



},{"./src/math-tooltip.coffee":19,"katex":2,"quill":18}],2:[function(require,module,exports){
/**
 * This is the main entry point for KaTeX. Here, we expose functions for
 * rendering expressions either to DOM nodes or to markup strings.
 *
 * We also expose the ParseError class to check if errors thrown from KaTeX are
 * errors in the expression, or errors in javascript handling.
 */

var ParseError = require("./src/ParseError");

var buildTree = require("./src/buildTree");
var parseTree = require("./src/parseTree");
var utils = require("./src/utils");

/**
 * Parse and build an expression, and place that expression in the DOM node
 * given.
 */
var render = function(toParse, baseNode) {
    utils.clearNode(baseNode);

    var tree = parseTree(toParse);
    var node = buildTree(tree).toNode();

    baseNode.appendChild(node);
};

// KaTeX's styles don't work properly in quirks mode. Print out an error, and
// disable rendering.
if (typeof document !== "undefined") {
    if (document.compatMode !== "CSS1Compat") {
        typeof console !== "undefined" && console.warn(
            "Warning: KaTeX doesn't work in quirks mode. Make sure your " +
                "website has a suitable doctype.");

        render = function() {
            throw new ParseError("KaTeX doesn't work in quirks mode.");
        };
    }
}

/**
 * Parse and build an expression, and return the markup for that.
 */
var renderToString = function(toParse) {
    var tree = parseTree(toParse);
    return buildTree(tree).toMarkup();
};

module.exports = {
    render: render,
    renderToString: renderToString,
    ParseError: ParseError
};

},{"./src/ParseError":5,"./src/buildTree":9,"./src/parseTree":14,"./src/utils":16}],3:[function(require,module,exports){
/**
 * The Lexer class handles tokenizing the input in various ways. Since our
 * parser expects us to be able to backtrack, the lexer allows lexing from any
 * given starting point.
 *
 * Its main exposed function is the `lex` function, which takes a position to
 * lex from and a type of token to lex. It defers to the appropriate `_innerLex`
 * function.
 *
 * The various `_innerLex` functions perform the actual lexing of different
 * kinds.
 */

var ParseError = require("./ParseError");

// The main lexer class
function Lexer(input) {
    this._input = input;
}

// The resulting token returned from `lex`.
function Token(text, data, position) {
    this.text = text;
    this.data = data;
    this.position = position;
}

// "normal" types of tokens. These are tokens which can be matched by a simple
// regex
var mathNormals = [
    /^[/|@.""`0-9a-zA-Z]/, // ords
    /^[*+-]/, // bins
    /^[=<>:]/, // rels
    /^[,;]/, // punctuation
    /^['\^_{}]/, // misc
    /^[(\[]/, // opens
    /^[)\]?!]/, // closes
    /^~/ // spacing
];

// These are "normal" tokens like above, but should instead be parsed in text
// mode.
var textNormals = [
    /^[a-zA-Z0-9`!@*()-=+\[\]'";:?\/.,]/, // ords
    /^[{}]/, // grouping
    /^~/ // spacing
];

// Regexes for matching whitespace
var whitespaceRegex = /^\s*/;
var whitespaceConcatRegex = /^( +|\\  +)/;

// This regex matches any other TeX function, which is a backslash followed by a
// word or a single symbol
var anyFunc = /^\\(?:[a-zA-Z]+|.)/;

/**
 * This function lexes a single normal token. It takes a position, a list of
 * "normal" tokens to try, and whether it should completely ignore whitespace or
 * not.
 */
Lexer.prototype._innerLex = function(pos, normals, ignoreWhitespace) {
    var input = this._input.slice(pos);
    var whitespace;

    if (ignoreWhitespace) {
        // Get rid of whitespace.
        whitespace = input.match(whitespaceRegex)[0];
        pos += whitespace.length;
        input = input.slice(whitespace.length);
    } else {
        // Do the funky concatenation of whitespace that happens in text mode.
        whitespace = input.match(whitespaceConcatRegex);
        if (whitespace !== null) {
            return new Token(" ", null, pos + whitespace[0].length);
        }
    }

    // If there's no more input to parse, return an EOF token
    if (input.length === 0) {
        return new Token("EOF", null, pos);
    }

    var match;
    if ((match = input.match(anyFunc))) {
        // If we match a function token, return it
        return new Token(match[0], null, pos + match[0].length);
    } else {
        // Otherwise, we look through the normal token regexes and see if it's
        // one of them.
        for (var i = 0; i < normals.length; i++) {
            var normal = normals[i];

            if ((match = input.match(normal))) {
                // If it is, return it
                return new Token(
                    match[0], null, pos + match[0].length);
            }
        }
    }

    throw new ParseError("Unexpected character: '" + input[0] +
        "'", this, pos);
};

// A regex to match a CSS color (like #ffffff or BlueViolet)
var cssColor = /^(#[a-z0-9]+|[a-z]+)/i;

/**
 * This function lexes a CSS color.
 */
Lexer.prototype._innerLexColor = function(pos) {
    var input = this._input.slice(pos);

    // Ignore whitespace
    var whitespace = input.match(whitespaceRegex)[0];
    pos += whitespace.length;
    input = input.slice(whitespace.length);

    var match;
    if ((match = input.match(cssColor))) {
        // If we look like a color, return a color
        return new Token(match[0], null, pos + match[0].length);
    } else {
        throw new ParseError("Invalid color", this, pos);
    }
};

// A regex to match a dimension. Dimensions look like
// "1.2em" or ".4pt" or "1 ex"
var sizeRegex = /^(-?)\s*(\d+(?:\.\d*)?|\.\d+)\s*([a-z]{2})/;

/**
 * This function lexes a dimension.
 */
Lexer.prototype._innerLexSize = function(pos) {
    var input = this._input.slice(pos);

    // Ignore whitespace
    var whitespace = input.match(whitespaceRegex)[0];
    pos += whitespace.length;
    input = input.slice(whitespace.length);

    var match;
    if ((match = input.match(sizeRegex))) {
        var unit = match[3];
        // We only currently handle "em" and "ex" units
        if (unit !== "em" && unit !== "ex") {
            throw new ParseError("Invalid unit: '" + unit + "'", this, pos);
        }
        return new Token(match[0], {
                number: +(match[1] + match[2]),
                unit: unit
            }, pos + match[0].length);
    }

    throw new ParseError("Invalid size", this, pos);
};

/**
 * This function lexes a string of whitespace.
 */
Lexer.prototype._innerLexWhitespace = function(pos) {
    var input = this._input.slice(pos);

    var whitespace = input.match(whitespaceRegex)[0];
    pos += whitespace.length;

    return new Token(whitespace, null, pos);
};

/**
 * This function lexes a single token starting at `pos` and of the given mode.
 * Based on the mode, we defer to one of the `_innerLex` functions.
 */
Lexer.prototype.lex = function(pos, mode) {
    if (mode === "math") {
        return this._innerLex(pos, mathNormals, true);
    } else if (mode === "text") {
        return this._innerLex(pos, textNormals, false);
    } else if (mode === "color") {
        return this._innerLexColor(pos);
    } else if (mode === "size") {
        return this._innerLexSize(pos);
    } else if (mode === "whitespace") {
        return this._innerLexWhitespace(pos);
    }
};

module.exports = Lexer;

},{"./ParseError":5}],4:[function(require,module,exports){
/**
 * This file contains information about the options that the Parser carries
 * around with it while parsing. Data is held in an `Options` object, and when
 * recursing, a new `Options` object can be created with the `.with*` and
 * `.reset` functions.
 */

/**
 * This is the main options class. It contains the style, size, and color of the
 * current parse level. It also contains the style and size of the parent parse
 * level, so size changes can be handled efficiently.
 *
 * Each of the `.with*` and `.reset` functions passes its current style and size
 * as the parentStyle and parentSize of the new options class, so parent
 * handling is taken care of automatically.
 */
function Options(style, size, color, parentStyle, parentSize) {
    this.style = style;
    this.color = color;
    this.size = size;

    if (parentStyle === undefined) {
        parentStyle = style;
    }
    this.parentStyle = parentStyle;

    if (parentSize === undefined) {
        parentSize = size;
    }
    this.parentSize = parentSize;
}

/**
 * Create a new options object with the given style.
 */
Options.prototype.withStyle = function(style) {
    return new Options(style, this.size, this.color, this.style, this.size);
};

/**
 * Create a new options object with the given size.
 */
Options.prototype.withSize = function(size) {
    return new Options(this.style, size, this.color, this.style, this.size);
};

/**
 * Create a new options object with the given color.
 */
Options.prototype.withColor = function(color) {
    return new Options(this.style, this.size, color, this.style, this.size);
};

/**
 * Create a new options object with the same style, size, and color. This is
 * used so that parent style and size changes are handled correctly.
 */
Options.prototype.reset = function() {
    return new Options(
        this.style, this.size, this.color, this.style, this.size);
};

/**
 * A map of color names to CSS colors.
 * TODO(emily): Remove this when we have real macros
 */
var colorMap = {
    "katex-blue": "#6495ed",
    "katex-orange": "#ffa500",
    "katex-pink": "#ff00af",
    "katex-red": "#df0030",
    "katex-green": "#28ae7b",
    "katex-gray": "gray",
    "katex-purple": "#9d38bd"
};

/**
 * Gets the CSS color of the current options object, accounting for the
 * `colorMap`.
 */
Options.prototype.getColor = function() {
    return colorMap[this.color] || this.color;
};

module.exports = Options;

},{}],5:[function(require,module,exports){
/**
 * This is the ParseError class, which is the main error thrown by KaTeX
 * functions when something has gone wrong. This is used to distinguish internal
 * errors from errors in the expression that the user provided.
 */
function ParseError(message, lexer, position) {
    var error = "KaTeX parse error: " + message;

    if (lexer !== undefined && position !== undefined) {
        // If we have the input and a position, make the error a bit fancier

        // Prepend some information
        error += " at position " + position + ": ";

        // Get the input
        var input = lexer._input;
        // Insert a combining underscore at the correct position
        input = input.slice(0, position) + "\u0332" +
            input.slice(position);

        // Extract some context from the input and add it to the error
        var begin = Math.max(0, position - 15);
        var end = position + 15;
        error += input.slice(begin, end);
    }

    // Some hackery to make ParseError a prototype of Error
    // See http://stackoverflow.com/a/8460753
    var self = new Error(error);
    self.name = "ParseError";
    self.__proto__ = ParseError.prototype;

    self.position = position;
    return self;
}

// More hackery
ParseError.prototype.__proto__ = Error.prototype;

module.exports = ParseError;

},{}],6:[function(require,module,exports){
var functions = require("./functions");
var Lexer = require("./Lexer");
var symbols = require("./symbols");
var utils = require("./utils");

var ParseError = require("./ParseError");

/**
 * This file contains the parser used to parse out a TeX expression from the
 * input. Since TeX isn't context-free, standard parsers don't work particularly
 * well.
 *
 * The strategy of this parser is as such:
 *
 * The main functions (the `.parse...` ones) take a position in the current
 * parse string to parse tokens from. The lexer (found in Lexer.js, stored at
 * this.lexer) also supports pulling out tokens at arbitrary places. When
 * individual tokens are needed at a position, the lexer is called to pull out a
 * token, which is then used.
 *
 * The main functions also take a mode that the parser is currently in
 * (currently "math" or "text"), which denotes whether the current environment
 * is a math-y one or a text-y one (e.g. inside \text). Currently, this serves
 * to limit the functions which can be used in text mode.
 *
 * The main functions then return an object which contains the useful data that
 * was parsed at its given point, and a new position at the end of the parsed
 * data. The main functions can call each other and continue the parsing by
 * using the returned position as a new starting point.
 *
 * There are also extra `.handle...` functions, which pull out some reused
 * functionality into self-contained functions.
 *
 * The earlier functions return `ParseResult`s, which contain a ParseNode and a
 * new position.
 *
 * The later functions (which are called deeper in the parse) sometimes return
 * ParseFuncOrArgument, which contain a ParseResult as well as some data about
 * whether the parsed object is a function which is missing some arguments, or a
 * standalone object which can be used as an argument to another function.
 */

/**
 * Main Parser class
 */
function Parser(input) {
    // Make a new lexer
    this.lexer = new Lexer(input);
}

/**
 * The resulting parse tree nodes of the parse tree.
 */
function ParseNode(type, value, mode) {
    this.type = type;
    this.value = value;
    this.mode = mode;
}

/**
 * A result and final position returned by the `.parse...` functions.
 */
function ParseResult(result, newPosition) {
    this.result = result;
    this.position = newPosition;
}

/**
 * An initial function (without its arguments), or an argument to a function.
 * The `result` argument should be a ParseResult.
 */
function ParseFuncOrArgument(result, isFunction, allowedInText, numArgs, numOptionalArgs, argTypes) {
    this.result = result;
    // Is this a function (i.e. is it something defined in functions.js)?
    this.isFunction = isFunction;
    // Is it allowed in text mode?
    this.allowedInText = allowedInText;
    // How many arguments?
    this.numArgs = numArgs;
    // How many optional arguments?
    this.numOptionalArgs = numOptionalArgs;
    // What types of arguments?
    this.argTypes = argTypes;
}

/**
 * Checks a result to make sure it has the right type, and throws an
 * appropriate error otherwise.
 */
Parser.prototype.expect = function(result, text) {
    if (result.text !== text) {
        throw new ParseError(
            "Expected '" + text + "', got '" + result.text + "'",
            this.lexer, result.position
        );
    }
};

/**
 * Main parsing function, which parses an entire input.
 *
 * @return {?Array.<ParseNode>}
 */
Parser.prototype.parse = function(input) {
    // Try to parse the input
    var parse = this.parseInput(0, "math");
    return parse.result;
};

/**
 * Parses an entire input tree.
 */
Parser.prototype.parseInput = function(pos, mode) {
    // Parse an expression
    var expression = this.parseExpression(pos, mode, false, null);
    // If we succeeded, make sure there's an EOF at the end
    var EOF = this.lexer.lex(expression.position, mode);
    this.expect(EOF, "EOF");
    return expression;
};

/**
 * Parses an "expression", which is a list of atoms.
 *
 * @param {boolean} breakOnInfix Should the parsing stop when we hit infix
 *                  nodes? This happens when functions have higher precendence
 *                  than infix nodes in implicit parses.
 *
 * @param {?string} breakOnToken The token that the expression should end with,
 *                  or `null` if something else should end the expression.
 *
 * @return {ParseResult}
 */
Parser.prototype.parseExpression = function(pos, mode, breakOnInfix, breakOnToken) {
    var body = [];
    // Keep adding atoms to the body until we can't parse any more atoms (either
    // we reached the end, a }, or a \right)
    while (true) {
        var lex = this.lexer.lex(pos, mode);
        if (breakOnToken != null && lex.text === breakOnToken) {
            break;
        }
        var atom = this.parseAtom(pos, mode);
        if (!atom) {
            break;
        }
        if (breakOnInfix && atom.result.type === "infix") {
            break;
        }
        body.push(atom.result);
        pos = atom.position;
    }
    return new ParseResult(this.handleInfixNodes(body, mode), pos);
};

/**
 * Rewrites infix operators such as \over with corresponding commands such
 * as \frac.
 *
 * There can only be one infix operator per group.  If there's more than one
 * then the expression is ambiguous.  This can be resolved by adding {}.
 *
 * @returns {Array}
 */
Parser.prototype.handleInfixNodes = function (body, mode) {
    var overIndex = -1;
    var func;
    var funcName;

    for (var i = 0; i < body.length; i++) {
        var node = body[i];
        if (node.type === "infix") {
            if (overIndex !== -1) {
                throw new ParseError("only one infix operator per group",
                    this.lexer, -1);
            }
            overIndex = i;
            funcName = node.value.replaceWith;
            func = functions.funcs[funcName];
        }
    }

    if (overIndex !== -1) {
        var numerNode, denomNode;

        var numerBody = body.slice(0, overIndex);
        var denomBody = body.slice(overIndex + 1);

        if (numerBody.length === 1 && numerBody[0].type === "ordgroup") {
            numerNode = numerBody[0];
        } else {
            numerNode = new ParseNode("ordgroup", numerBody, mode);
        }

        if (denomBody.length === 1 && denomBody[0].type === "ordgroup") {
            denomNode = denomBody[0];
        } else {
            denomNode = new ParseNode("ordgroup", denomBody, mode);
        }

        var value = func.handler(funcName, numerNode, denomNode);
        return [new ParseNode(value.type, value, mode)];
    } else {
        return body;
    }
};

// The greediness of a superscript or subscript
var SUPSUB_GREEDINESS = 1;

/**
 * Handle a subscript or superscript with nice errors.
 */
Parser.prototype.handleSupSubscript = function(pos, mode, symbol, name) {
    var group = this.parseGroup(pos, mode);

    if (!group) {
        throw new ParseError(
            "Expected group after '" + symbol + "'", this.lexer, pos);
    } else if (group.numArgs > 0) {
        // ^ and _ have a greediness, so handle interactions with functions'
        // greediness
        var funcGreediness = functions.getGreediness(group.result.result);
        if (funcGreediness > SUPSUB_GREEDINESS) {
            return this.parseFunction(pos, mode);
        } else {
            throw new ParseError(
                "Got function '" + group.result.result + "' with no arguments " +
                    "as " + name,
                this.lexer, pos);
        }
    } else {
        return group.result;
    }
};

/**
 * Parses a group with optional super/subscripts.
 *
 * @return {?ParseResult}
 */
Parser.prototype.parseAtom = function(pos, mode) {
    // The body of an atom is an implicit group, so that things like
    // \left(x\right)^2 work correctly.
    var base = this.parseImplicitGroup(pos, mode);

    // In text mode, we don't have superscripts or subscripts
    if (mode === "text") {
        return base;
    }

    // Handle an empty base
    var currPos;
    if (!base) {
        currPos = pos;
        base = undefined;
    } else {
        currPos = base.position;
    }

    var superscript;
    var subscript;
    var result;
    while (true) {
        // Lex the first token
        var lex = this.lexer.lex(currPos, mode);

        if (lex.text === "^") {
            // We got a superscript start
            if (superscript) {
                throw new ParseError(
                    "Double superscript", this.lexer, currPos);
            }
            result = this.handleSupSubscript(
                lex.position, mode, lex.text, "superscript");
            currPos = result.position;
            superscript = result.result;
        } else if (lex.text === "_") {
            // We got a subscript start
            if (subscript) {
                throw new ParseError(
                    "Double subscript", this.lexer, currPos);
            }
            result = this.handleSupSubscript(
                lex.position, mode, lex.text, "subscript");
            currPos = result.position;
            subscript = result.result;
        } else if (lex.text === "'") {
            // We got a prime
            var prime = new ParseNode("textord", "\\prime", mode);

            // Many primes can be grouped together, so we handle this here
            var primes = [prime];
            currPos = lex.position;
            // Keep lexing tokens until we get something that's not a prime
            while ((lex = this.lexer.lex(currPos, mode)).text === "'") {
                // For each one, add another prime to the list
                primes.push(prime);
                currPos = lex.position;
            }
            // Put them into an ordgroup as the superscript
            superscript = new ParseNode("ordgroup", primes, mode);
        } else {
            // If it wasn't ^, _, or ', stop parsing super/subscripts
            break;
        }
    }

    if (superscript || subscript) {
        // If we got either a superscript or subscript, create a supsub
        return new ParseResult(
            new ParseNode("supsub", {
                base: base && base.result,
                sup: superscript,
                sub: subscript
            }, mode),
            currPos);
    } else {
        // Otherwise return the original body
        return base;
    }
};

// A list of the size-changing functions, for use in parseImplicitGroup
var sizeFuncs = [
    "\\tiny", "\\scriptsize", "\\footnotesize", "\\small", "\\normalsize",
    "\\large", "\\Large", "\\LARGE", "\\huge", "\\Huge"
];

// A list of the style-changing functions, for use in parseImplicitGroup
var styleFuncs = [
    "\\displaystyle", "\\textstyle", "\\scriptstyle", "\\scriptscriptstyle"
];

/**
 * Parses an implicit group, which is a group that starts at the end of a
 * specified, and ends right before a higher explicit group ends, or at EOL. It
 * is used for functions that appear to affect the current style, like \Large or
 * \textrm, where instead of keeping a style we just pretend that there is an
 * implicit grouping after it until the end of the group. E.g.
 *   small text {\Large large text} small text again
 * It is also used for \left and \right to get the correct grouping.
 *
 * @return {?ParseResult}
 */
Parser.prototype.parseImplicitGroup = function(pos, mode) {
    var start = this.parseSymbol(pos, mode);

    if (!start || !start.result) {
        // If we didn't get anything we handle, fall back to parseFunction
        return this.parseFunction(pos, mode);
    }

    var func = start.result.result;
    var body;

    if (func === "\\left") {
        // If we see a left:
        // Parse the entire left function (including the delimiter)
        var left = this.parseFunction(pos, mode);
        // Parse out the implicit body
        body = this.parseExpression(left.position, mode, false, "}");
        // Check the next token
        var rightLex = this.parseSymbol(body.position, mode);

        if (rightLex && rightLex.result.result === "\\right") {
            // If it's a \right, parse the entire right function (including the delimiter)
            var right = this.parseFunction(body.position, mode);

            return new ParseResult(
                new ParseNode("leftright", {
                    body: body.result,
                    left: left.result.value.value,
                    right: right.result.value.value
                }, mode),
                right.position);
        } else {
            throw new ParseError("Missing \\right", this.lexer, body.position);
        }
    } else if (func === "\\right") {
        // If we see a right, explicitly fail the parsing here so the \left
        // handling ends the group
        return null;
    } else if (utils.contains(sizeFuncs, func)) {
        // If we see a sizing function, parse out the implict body
        body = this.parseExpression(start.result.position, mode, false, "}");
        return new ParseResult(
            new ParseNode("sizing", {
                // Figure out what size to use based on the list of functions above
                size: "size" + (utils.indexOf(sizeFuncs, func) + 1),
                value: body.result
            }, mode),
            body.position);
    } else if (utils.contains(styleFuncs, func)) {
        // If we see a styling function, parse out the implict body
        body = this.parseExpression(start.result.position, mode, true, "}");
        return new ParseResult(
            new ParseNode("styling", {
                // Figure out what style to use by pulling out the style from
                // the function name
                style: func.slice(1, func.length - 5),
                value: body.result
            }, mode),
            body.position);
    } else {
        // Defer to parseFunction if it's not a function we handle
        return this.parseFunction(pos, mode);
    }
};

/**
 * Parses an entire function, including its base and all of its arguments
 *
 * @return {?ParseResult}
 */
Parser.prototype.parseFunction = function(pos, mode) {
    var baseGroup = this.parseGroup(pos, mode);

    if (baseGroup) {
        if (baseGroup.isFunction) {
            var func = baseGroup.result.result;
            if (mode === "text" && !baseGroup.allowedInText) {
                throw new ParseError(
                    "Can't use function '" + func + "' in text mode",
                    this.lexer, baseGroup.position);
            }

            var newPos = baseGroup.result.position;
            var result;

            var totalArgs = baseGroup.numArgs + baseGroup.numOptionalArgs;

            if (totalArgs > 0) {
                var baseGreediness = functions.getGreediness(func);
                var args = [func];
                var positions = [newPos];

                for (var i = 0; i < totalArgs; i++) {
                    var argType = baseGroup.argTypes && baseGroup.argTypes[i];
                    var arg;
                    if (i < baseGroup.numOptionalArgs) {
                        if (argType) {
                            arg = this.parseSpecialGroup(newPos, argType, mode, true);
                        } else {
                            arg = this.parseOptionalGroup(newPos, mode);
                        }
                        if (!arg) {
                            args.push(null);
                            positions.push(newPos);
                            continue;
                        }
                    } else {
                        if (argType) {
                            arg = this.parseSpecialGroup(newPos, argType, mode);
                        } else {
                            arg = this.parseGroup(newPos, mode);
                        }
                        if (!arg) {
                            throw new ParseError(
                                "Expected group after '" + baseGroup.result.result +
                                    "'",
                                this.lexer, newPos);
                        }
                    }
                    var argNode;
                    if (arg.numArgs > 0) {
                        var argGreediness = functions.getGreediness(arg.result.result);
                        if (argGreediness > baseGreediness) {
                            argNode = this.parseFunction(newPos, mode);
                        } else {
                            throw new ParseError(
                                "Got function '" + arg.result.result + "' as " +
                                    "argument to function '" +
                                    baseGroup.result.result + "'",
                                this.lexer, arg.result.position - 1);
                        }
                    } else {
                        argNode = arg.result;
                    }
                    args.push(argNode.result);
                    positions.push(argNode.position);
                    newPos = argNode.position;
                }

                args.push(positions);

                result = functions.funcs[func].handler.apply(this, args);
            } else {
                result = functions.funcs[func].handler.apply(this, [func]);
            }

            return new ParseResult(
                new ParseNode(result.type, result, mode),
                newPos);
        } else {
            return baseGroup.result;
        }
    } else {
        return null;
    }
};

/**
 * Parses a group when the mode is changing. Takes a position, a new mode, and
 * an outer mode that is used to parse the outside.
 *
 * @return {?ParseFuncOrArgument}
 */
Parser.prototype.parseSpecialGroup = function(pos, mode, outerMode, optional) {
    if (mode === "color" || mode === "size") {
        // color and size modes are special because they should have braces and
        // should only lex a single symbol inside
        var openBrace = this.lexer.lex(pos, outerMode);
        if (optional && openBrace.text !== "[") {
            // optional arguments should return null if they don't exist
            return null;
        }
        this.expect(openBrace, optional ? "[" : "{");
        var inner = this.lexer.lex(openBrace.position, mode);
        var data;
        if (mode === "color") {
            data = inner.text;
        } else {
            data = inner.data;
        }
        var closeBrace = this.lexer.lex(inner.position, outerMode);
        this.expect(closeBrace, optional ? "]" : "}");
        return new ParseFuncOrArgument(
            new ParseResult(
                new ParseNode(mode, data, outerMode),
                closeBrace.position),
            false);
    } else if (mode === "text") {
        // text mode is special because it should ignore the whitespace before
        // it
        var whitespace = this.lexer.lex(pos, "whitespace");
        pos = whitespace.position;
    }

    if (optional) {
        return this.parseOptionalGroup(pos, mode);
    } else {
        return this.parseGroup(pos, mode);
    }
};

/**
 * Parses a group, which is either a single nucleus (like "x") or an expression
 * in braces (like "{x+y}")
 *
 * @return {?ParseFuncOrArgument}
 */
Parser.prototype.parseGroup = function(pos, mode) {
    var start = this.lexer.lex(pos, mode);
    // Try to parse an open brace
    if (start.text === "{") {
        // If we get a brace, parse an expression
        var expression = this.parseExpression(start.position, mode, false, "}");
        // Make sure we get a close brace
        var closeBrace = this.lexer.lex(expression.position, mode);
        this.expect(closeBrace, "}");
        return new ParseFuncOrArgument(
            new ParseResult(
                new ParseNode("ordgroup", expression.result, mode),
                closeBrace.position),
            false);
    } else {
        // Otherwise, just return a nucleus
        return this.parseSymbol(pos, mode);
    }
};

/**
 * Parses a group, which is an expression in brackets (like "[x+y]")
 *
 * @return {?ParseFuncOrArgument}
 */
Parser.prototype.parseOptionalGroup = function(pos, mode) {
    var start = this.lexer.lex(pos, mode);
    // Try to parse an open bracket
    if (start.text === "[") {
        // If we get a brace, parse an expression
        var expression = this.parseExpression(start.position, mode, false, "]");
        // Make sure we get a close bracket
        var closeBracket = this.lexer.lex(expression.position, mode);
        this.expect(closeBracket, "]");
        return new ParseFuncOrArgument(
            new ParseResult(
                new ParseNode("ordgroup", expression.result, mode),
                closeBracket.position),
            false);
    } else {
        // Otherwise, return null,
        return null;
    }
};

/**
 * Parse a single symbol out of the string. Here, we handle both the functions
 * we have defined, as well as the single character symbols
 *
 * @return {?ParseFuncOrArgument}
 */
Parser.prototype.parseSymbol = function(pos, mode) {
    var nucleus = this.lexer.lex(pos, mode);

    if (functions.funcs[nucleus.text]) {
        // If there is a function with this name, we use its data
        var func = functions.funcs[nucleus.text];

        // Here, we replace "original" argTypes with the current mode
        var argTypes = func.argTypes;
        if (argTypes) {
            argTypes = argTypes.slice();
            for (var i = 0; i < argTypes.length; i++) {
                if (argTypes[i] === "original") {
                    argTypes[i] = mode;
                }
            }
        }

        return new ParseFuncOrArgument(
            new ParseResult(nucleus.text, nucleus.position),
            true, func.allowedInText, func.numArgs, func.numOptionalArgs, argTypes);
    } else if (symbols[mode][nucleus.text]) {
        // Otherwise if this is a no-argument function, find the type it
        // corresponds to in the symbols map
        return new ParseFuncOrArgument(
            new ParseResult(
                new ParseNode(symbols[mode][nucleus.text].group,
                              nucleus.text, mode),
                nucleus.position),
            false);
    } else {
        return null;
    }
};

module.exports = Parser;

},{"./Lexer":3,"./ParseError":5,"./functions":13,"./symbols":15,"./utils":16}],7:[function(require,module,exports){
/**
 * This file contains information and classes for the various kinds of styles
 * used in TeX. It provides a generic `Style` class, which holds information
 * about a specific style. It then provides instances of all the different kinds
 * of styles possible, and provides functions to move between them and get
 * information about them.
 */

/**
 * The main style class. Contains a unique id for the style, a size (which is
 * the same for cramped and uncramped version of a style), a cramped flag, and a
 * size multiplier, which gives the size difference between a style and
 * textstyle.
 */
function Style(id, size, multiplier, cramped) {
    this.id = id;
    this.size = size;
    this.cramped = cramped;
    this.sizeMultiplier = multiplier;
}

/**
 * Get the style of a superscript given a base in the current style.
 */
Style.prototype.sup = function() {
    return styles[sup[this.id]];
};

/**
 * Get the style of a subscript given a base in the current style.
 */
Style.prototype.sub = function() {
    return styles[sub[this.id]];
};

/**
 * Get the style of a fraction numerator given the fraction in the current
 * style.
 */
Style.prototype.fracNum = function() {
    return styles[fracNum[this.id]];
};

/**
 * Get the style of a fraction denominator given the fraction in the current
 * style.
 */
Style.prototype.fracDen = function() {
    return styles[fracDen[this.id]];
};

/**
 * Get the cramped version of a style (in particular, cramping a cramped style
 * doesn't change the style).
 */
Style.prototype.cramp = function() {
    return styles[cramp[this.id]];
};

/**
 * HTML class name, like "displaystyle cramped"
 */
Style.prototype.cls = function() {
    return sizeNames[this.size] + (this.cramped ? " cramped" : " uncramped");
};

/**
 * HTML Reset class name, like "reset-textstyle"
 */
Style.prototype.reset = function() {
    return resetNames[this.size];
};

// IDs of the different styles
var D = 0;
var Dc = 1;
var T = 2;
var Tc = 3;
var S = 4;
var Sc = 5;
var SS = 6;
var SSc = 7;

// String names for the different sizes
var sizeNames = [
    "displaystyle textstyle",
    "textstyle",
    "scriptstyle",
    "scriptscriptstyle"
];

// Reset names for the different sizes
var resetNames = [
    "reset-textstyle",
    "reset-textstyle",
    "reset-scriptstyle",
    "reset-scriptscriptstyle"
];

// Instances of the different styles
var styles = [
    new Style(D, 0, 1.0, false),
    new Style(Dc, 0, 1.0, true),
    new Style(T, 1, 1.0, false),
    new Style(Tc, 1, 1.0, true),
    new Style(S, 2, 0.7, false),
    new Style(Sc, 2, 0.7, true),
    new Style(SS, 3, 0.5, false),
    new Style(SSc, 3, 0.5, true)
];

// Lookup tables for switching from one style to another
var sup = [S, Sc, S, Sc, SS, SSc, SS, SSc];
var sub = [Sc, Sc, Sc, Sc, SSc, SSc, SSc, SSc];
var fracNum = [T, Tc, S, Sc, SS, SSc, SS, SSc];
var fracDen = [Tc, Tc, Sc, Sc, SSc, SSc, SSc, SSc];
var cramp = [Dc, Dc, Tc, Tc, Sc, Sc, SSc, SSc];

// We only export some of the styles. Also, we don't export the `Style` class so
// no more styles can be generated.
module.exports = {
    DISPLAY: styles[D],
    TEXT: styles[T],
    SCRIPT: styles[S],
    SCRIPTSCRIPT: styles[SS]
};

},{}],8:[function(require,module,exports){
/**
 * This module contains general functions that can be used for building
 * different kinds of domTree nodes in a consistent manner.
 */

var domTree = require("./domTree");
var fontMetrics = require("./fontMetrics");
var symbols = require("./symbols");

/**
 * Makes a symbolNode after translation via the list of symbols in symbols.js.
 * Correctly pulls out metrics for the character, and optionally takes a list of
 * classes to be attached to the node.
 */
var makeSymbol = function(value, style, mode, color, classes) {
    // Replace the value with its replaced value from symbol.js
    if (symbols[mode][value] && symbols[mode][value].replace) {
        value = symbols[mode][value].replace;
    }

    var metrics = fontMetrics.getCharacterMetrics(value, style);

    var symbolNode;
    if (metrics) {
        symbolNode = new domTree.symbolNode(
            value, metrics.height, metrics.depth, metrics.italic, metrics.skew,
            classes);
    } else {
        // TODO(emily): Figure out a good way to only print this in development
        typeof console !== "undefined" && console.warn(
            "No character metrics for '" + value + "' in style '" +
                style + "'");
        symbolNode = new domTree.symbolNode(value, 0, 0, 0, 0, classes);
    }

    if (color) {
        symbolNode.style.color = color;
    }

    return symbolNode;
};

/**
 * Makes a symbol in the italic math font.
 */
var mathit = function(value, mode, color, classes) {
    return makeSymbol(
        value, "Math-Italic", mode, color, classes.concat(["mathit"]));
};

/**
 * Makes a symbol in the upright roman font.
 */
var mathrm = function(value, mode, color, classes) {
    // Decide what font to render the symbol in by its entry in the symbols
    // table.
    if (symbols[mode][value].font === "main") {
        return makeSymbol(value, "Main-Regular", mode, color, classes);
    } else {
        return makeSymbol(
            value, "AMS-Regular", mode, color, classes.concat(["amsrm"]));
    }
};

/**
 * Calculate the height, depth, and maxFontSize of an element based on its
 * children.
 */
var sizeElementFromChildren = function(elem) {
    var height = 0;
    var depth = 0;
    var maxFontSize = 0;

    if (elem.children) {
        for (var i = 0; i < elem.children.length; i++) {
            if (elem.children[i].height > height) {
                height = elem.children[i].height;
            }
            if (elem.children[i].depth > depth) {
                depth = elem.children[i].depth;
            }
            if (elem.children[i].maxFontSize > maxFontSize) {
                maxFontSize = elem.children[i].maxFontSize;
            }
        }
    }

    elem.height = height;
    elem.depth = depth;
    elem.maxFontSize = maxFontSize;
};

/**
 * Makes a span with the given list of classes, list of children, and color.
 */
var makeSpan = function(classes, children, color) {
    var span = new domTree.span(classes, children);

    sizeElementFromChildren(span);

    if (color) {
        span.style.color = color;
    }

    return span;
};

/**
 * Makes a document fragment with the given list of children.
 */
var makeFragment = function(children) {
    var fragment = new domTree.documentFragment(children);

    sizeElementFromChildren(fragment);

    return fragment;
};

/**
 * Makes an element placed in each of the vlist elements to ensure that each
 * element has the same max font size. To do this, we create a zero-width space
 * with the correct font size.
 */
var makeFontSizer = function(options, fontSize) {
    var fontSizeInner = makeSpan([], [new domTree.symbolNode("\u200b")]);
    fontSizeInner.style.fontSize = (fontSize / options.style.sizeMultiplier) + "em";

    var fontSizer = makeSpan(
        ["fontsize-ensurer", "reset-" + options.size, "size5"],
        [fontSizeInner]);

    return fontSizer;
};

/**
 * Makes a vertical list by stacking elements and kerns on top of each other.
 * Allows for many different ways of specifying the positioning method.
 *
 * Arguments:
 *  - children: A list of child or kern nodes to be stacked on top of each other
 *              (i.e. the first element will be at the bottom, and the last at
 *              the top). Element nodes are specified as
 *                {type: "elem", elem: node}
 *              while kern nodes are specified as
 *                {type: "kern", size: size}
 *  - positionType: The method by which the vlist should be positioned. Valid
 *                  values are:
 *                   - "individualShift": The children list only contains elem
 *                                        nodes, and each node contains an extra
 *                                        "shift" value of how much it should be
 *                                        shifted (note that shifting is always
 *                                        moving downwards). positionData is
 *                                        ignored.
 *                   - "top": The positionData specifies the topmost point of
 *                            the vlist (note this is expected to be a height,
 *                            so positive values move up)
 *                   - "bottom": The positionData specifies the bottommost point
 *                               of the vlist (note this is expected to be a
 *                               depth, so positive values move down
 *                   - "shift": The vlist will be positioned such that its
 *                              baseline is positionData away from the baseline
 *                              of the first child. Positive values move
 *                              downwards.
 *                   - "firstBaseline": The vlist will be positioned such that
 *                                      its baseline is aligned with the
 *                                      baseline of the first child.
 *                                      positionData is ignored. (this is
 *                                      equivalent to "shift" with
 *                                      positionData=0)
 *  - positionData: Data used in different ways depending on positionType
 *  - options: An Options object
 *
 */
var makeVList = function(children, positionType, positionData, options) {
    var depth;
    var currPos;
    var i;
    if (positionType === "individualShift") {
        var oldChildren = children;
        children = [oldChildren[0]];

        // Add in kerns to the list of children to get each element to be
        // shifted to the correct specified shift
        depth = -oldChildren[0].shift - oldChildren[0].elem.depth;
        currPos = depth;
        for (i = 1; i < oldChildren.length; i++) {
            var diff = -oldChildren[i].shift - currPos -
                oldChildren[i].elem.depth;
            var size = diff -
                (oldChildren[i - 1].elem.height +
                 oldChildren[i - 1].elem.depth);

            currPos = currPos + diff;

            children.push({type: "kern", size: size});
            children.push(oldChildren[i]);
        }
    } else if (positionType === "top") {
        // We always start at the bottom, so calculate the bottom by adding up
        // all the sizes
        var bottom = positionData;
        for (i = 0; i < children.length; i++) {
            if (children[i].type === "kern") {
                bottom -= children[i].size;
            } else {
                bottom -= children[i].elem.height + children[i].elem.depth;
            }
        }
        depth = bottom;
    } else if (positionType === "bottom") {
        depth = -positionData;
    } else if (positionType === "shift") {
        depth = -children[0].elem.depth - positionData;
    } else if (positionType === "firstBaseline") {
        depth = -children[0].elem.depth;
    } else {
        depth = 0;
    }

    // Make the fontSizer
    var maxFontSize = 0;
    for (i = 0; i < children.length; i++) {
        if (children[i].type === "elem") {
            maxFontSize = Math.max(maxFontSize, children[i].elem.maxFontSize);
        }
    }
    var fontSizer = makeFontSizer(options, maxFontSize);

    // Create a new list of actual children at the correct offsets
    var realChildren = [];
    currPos = depth;
    for (i = 0; i < children.length; i++) {
        if (children[i].type === "kern") {
            currPos += children[i].size;
        } else {
            var child = children[i].elem;

            var shift = -child.depth - currPos;
            currPos += child.height + child.depth;

            var childWrap = makeSpan([], [fontSizer, child]);
            childWrap.height -= shift;
            childWrap.depth += shift;
            childWrap.style.top = shift + "em";

            realChildren.push(childWrap);
        }
    }

    // Add in an element at the end with no offset to fix the calculation of
    // baselines in some browsers (namely IE, sometimes safari)
    var baselineFix = makeSpan(
        ["baseline-fix"], [fontSizer, new domTree.symbolNode("\u200b")]);
    realChildren.push(baselineFix);

    var vlist = makeSpan(["vlist"], realChildren);
    // Fix the final height and depth, in case there were kerns at the ends
    // since the makeSpan calculation won't take that in to account.
    vlist.height = Math.max(currPos, vlist.height);
    vlist.depth = Math.max(-depth, vlist.depth);
    return vlist;
};

module.exports = {
    makeSymbol: makeSymbol,
    mathit: mathit,
    mathrm: mathrm,
    makeSpan: makeSpan,
    makeFragment: makeFragment,
    makeVList: makeVList
};

},{"./domTree":11,"./fontMetrics":12,"./symbols":15}],9:[function(require,module,exports){
/**
 * This file does the main work of building a domTree structure from a parse
 * tree. The entry point is the `buildTree` function, which takes a parse tree.
 * Then, the buildExpression, buildGroup, and various groupTypes functions are
 * called, to produce a final tree.
 */

var Options = require("./Options");
var ParseError = require("./ParseError");
var Style = require("./Style");

var buildCommon = require("./buildCommon");
var delimiter = require("./delimiter");
var domTree = require("./domTree");
var fontMetrics = require("./fontMetrics");
var utils = require("./utils");

var makeSpan = buildCommon.makeSpan;

/**
 * Take a list of nodes, build them in order, and return a list of the built
 * nodes. This function handles the `prev` node correctly, and passes the
 * previous element from the list as the prev of the next element.
 */
var buildExpression = function(expression, options, prev) {
    var groups = [];
    for (var i = 0; i < expression.length; i++) {
        var group = expression[i];
        groups.push(buildGroup(group, options, prev));
        prev = group;
    }
    return groups;
};

// List of types used by getTypeOfGroup
var groupToType = {
    mathord: "mord",
    textord: "mord",
    bin: "mbin",
    rel: "mrel",
    text: "mord",
    open: "mopen",
    close: "mclose",
    inner: "minner",
    frac: "minner",
    spacing: "mord",
    punct: "mpunct",
    ordgroup: "mord",
    op: "mop",
    katex: "mord",
    overline: "mord",
    rule: "mord",
    leftright: "minner",
    sqrt: "mord",
    accent: "mord"
};

/**
 * Gets the final math type of an expression, given its group type. This type is
 * used to determine spacing between elements, and affects bin elements by
 * causing them to change depending on what types are around them. This type
 * must be attached to the outermost node of an element as a CSS class so that
 * spacing with its surrounding elements works correctly.
 *
 * Some elements can be mapped one-to-one from group type to math type, and
 * those are listed in the `groupToType` table.
 *
 * Others (usually elements that wrap around other elements) often have
 * recursive definitions, and thus call `getTypeOfGroup` on their inner
 * elements.
 */
var getTypeOfGroup = function(group) {
    if (group == null) {
        // Like when typesetting $^3$
        return groupToType.mathord;
    } else if (group.type === "supsub") {
        return getTypeOfGroup(group.value.base);
    } else if (group.type === "llap" || group.type === "rlap") {
        return getTypeOfGroup(group.value);
    } else if (group.type === "color") {
        return getTypeOfGroup(group.value.value);
    } else if (group.type === "sizing") {
        return getTypeOfGroup(group.value.value);
    } else if (group.type === "styling") {
        return getTypeOfGroup(group.value.value);
    } else if (group.type === "delimsizing") {
        return groupToType[group.value.delimType];
    } else {
        return groupToType[group.type];
    }
};

/**
 * Sometimes, groups perform special rules when they have superscripts or
 * subscripts attached to them. This function lets the `supsub` group know that
 * its inner element should handle the superscripts and subscripts instead of
 * handling them itself.
 */
var shouldHandleSupSub = function(group, options) {
    if (!group) {
        return false;
    } else if (group.type === "op") {
        // Operators handle supsubs differently when they have limits
        // (e.g. `\displaystyle\sum_2^3`)
        return group.value.limits && options.style.size === Style.DISPLAY.size;
    } else if (group.type === "accent") {
        return isCharacterBox(group.value.base);
    } else {
        return null;
    }
};

/**
 * Sometimes we want to pull out the innermost element of a group. In most
 * cases, this will just be the group itself, but when ordgroups and colors have
 * a single element, we want to pull that out.
 */
var getBaseElem = function(group) {
    if (!group) {
        return false;
    } else if (group.type === "ordgroup") {
        if (group.value.length === 1) {
            return getBaseElem(group.value[0]);
        } else {
            return group;
        }
    } else if (group.type === "color") {
        if (group.value.value.length === 1) {
            return getBaseElem(group.value.value[0]);
        } else {
            return group;
        }
    } else {
        return group;
    }
};

/**
 * TeXbook algorithms often reference "character boxes", which are simply groups
 * with a single character in them. To decide if something is a character box,
 * we find its innermost group, and see if it is a single character.
 */
var isCharacterBox = function(group) {
    var baseElem = getBaseElem(group);

    // These are all they types of groups which hold single characters
    return baseElem.type === "mathord" ||
        baseElem.type === "textord" ||
        baseElem.type === "bin" ||
        baseElem.type === "rel" ||
        baseElem.type === "inner" ||
        baseElem.type === "open" ||
        baseElem.type === "close" ||
        baseElem.type === "punct";
};

/**
 * This is a map of group types to the function used to handle that type.
 * Simpler types come at the beginning, while complicated types come afterwards.
 */
var groupTypes = {
    mathord: function(group, options, prev) {
        return buildCommon.mathit(
            group.value, group.mode, options.getColor(), ["mord"]);
    },

    textord: function(group, options, prev) {
        return buildCommon.mathrm(
            group.value, group.mode, options.getColor(), ["mord"]);
    },

    bin: function(group, options, prev) {
        var className = "mbin";
        // Pull out the most recent element. Do some special handling to find
        // things at the end of a \color group. Note that we don't use the same
        // logic for ordgroups (which count as ords).
        var prevAtom = prev;
        while (prevAtom && prevAtom.type == "color") {
            var atoms = prevAtom.value.value;
            prevAtom = atoms[atoms.length - 1];
        }
        // See TeXbook pg. 442-446, Rules 5 and 6, and the text before Rule 19.
        // Here, we determine whether the bin should turn into an ord. We
        // currently only apply Rule 5.
        if (!prev || utils.contains(["mbin", "mopen", "mrel", "mop", "mpunct"],
                getTypeOfGroup(prevAtom))) {
            group.type = "textord";
            className = "mord";
        }

        return buildCommon.mathrm(
            group.value, group.mode, options.getColor(), [className]);
    },

    rel: function(group, options, prev) {
        return buildCommon.mathrm(
            group.value, group.mode, options.getColor(), ["mrel"]);
    },

    open: function(group, options, prev) {
        return buildCommon.mathrm(
            group.value, group.mode, options.getColor(), ["mopen"]);
    },

    close: function(group, options, prev) {
        return buildCommon.mathrm(
            group.value, group.mode, options.getColor(), ["mclose"]);
    },

    inner: function(group, options, prev) {
        return buildCommon.mathrm(
            group.value, group.mode, options.getColor(), ["minner"]);
    },

    punct: function(group, options, prev) {
        return buildCommon.mathrm(
            group.value, group.mode, options.getColor(), ["mpunct"]);
    },

    ordgroup: function(group, options, prev) {
        return makeSpan(
            ["mord", options.style.cls()],
            buildExpression(group.value, options.reset())
        );
    },

    text: function(group, options, prev) {
        return makeSpan(["text", "mord", options.style.cls()],
            buildExpression(group.value.body, options.reset()));
    },

    color: function(group, options, prev) {
        var elements = buildExpression(
            group.value.value,
            options.withColor(group.value.color),
            prev
        );

        // \color isn't supposed to affect the type of the elements it contains.
        // To accomplish this, we wrap the results in a fragment, so the inner
        // elements will be able to directly interact with their neighbors. For
        // example, `\color{red}{2 +} 3` has the same spacing as `2 + 3`
        return new buildCommon.makeFragment(elements);
    },

    supsub: function(group, options, prev) {
        // Superscript and subscripts are handled in the TeXbook on page
        // 445-446, rules 18(a-f).

        // Here is where we defer to the inner group if it should handle
        // superscripts and subscripts itself.
        if (shouldHandleSupSub(group.value.base, options)) {
            return groupTypes[group.value.base.type](group, options, prev);
        }

        var base = buildGroup(group.value.base, options.reset());
        var supmid, submid, sup, sub;

        if (group.value.sup) {
            sup = buildGroup(group.value.sup,
                    options.withStyle(options.style.sup()));
            supmid = makeSpan(
                    [options.style.reset(), options.style.sup().cls()], [sup]);
        }

        if (group.value.sub) {
            sub = buildGroup(group.value.sub,
                    options.withStyle(options.style.sub()));
            submid = makeSpan(
                    [options.style.reset(), options.style.sub().cls()], [sub]);
        }

        // Rule 18a
        var supShift, subShift;
        if (isCharacterBox(group.value.base)) {
            supShift = 0;
            subShift = 0;
        } else {
            supShift = base.height - fontMetrics.metrics.supDrop;
            subShift = base.depth + fontMetrics.metrics.subDrop;
        }

        // Rule 18c
        var minSupShift;
        if (options.style === Style.DISPLAY) {
            minSupShift = fontMetrics.metrics.sup1;
        } else if (options.style.cramped) {
            minSupShift = fontMetrics.metrics.sup3;
        } else {
            minSupShift = fontMetrics.metrics.sup2;
        }

        // scriptspace is a font-size-independent size, so scale it
        // appropriately
        var multiplier = Style.TEXT.sizeMultiplier *
                options.style.sizeMultiplier;
        var scriptspace =
            (0.5 / fontMetrics.metrics.ptPerEm) / multiplier + "em";

        var supsub;
        if (!group.value.sup) {
            // Rule 18b
            subShift = Math.max(
                subShift, fontMetrics.metrics.sub1,
                sub.height - 0.8 * fontMetrics.metrics.xHeight);

            supsub = buildCommon.makeVList([
                {type: "elem", elem: submid}
            ], "shift", subShift, options);

            supsub.children[0].style.marginRight = scriptspace;

            // Subscripts shouldn't be shifted by the base's italic correction.
            // Account for that by shifting the subscript back the appropriate
            // amount. Note we only do this when the base is a single symbol.
            if (base instanceof domTree.symbolNode) {
                supsub.children[0].style.marginLeft = -base.italic + "em";
            }
        } else if (!group.value.sub) {
            // Rule 18c, d
            supShift = Math.max(supShift, minSupShift,
                sup.depth + 0.25 * fontMetrics.metrics.xHeight);

            supsub = buildCommon.makeVList([
                {type: "elem", elem: supmid}
            ], "shift", -supShift, options);

            supsub.children[0].style.marginRight = scriptspace;
        } else {
            supShift = Math.max(
                supShift, minSupShift,
                sup.depth + 0.25 * fontMetrics.metrics.xHeight);
            subShift = Math.max(subShift, fontMetrics.metrics.sub2);

            var ruleWidth = fontMetrics.metrics.defaultRuleThickness;

            // Rule 18e
            if ((supShift - sup.depth) - (sub.height - subShift) <
                    4 * ruleWidth) {
                subShift = 4 * ruleWidth - (supShift - sup.depth) + sub.height;
                var psi = 0.8 * fontMetrics.metrics.xHeight -
                    (supShift - sup.depth);
                if (psi > 0) {
                    supShift += psi;
                    subShift -= psi;
                }
            }

            supsub = buildCommon.makeVList([
                {type: "elem", elem: submid, shift: subShift},
                {type: "elem", elem: supmid, shift: -supShift}
            ], "individualShift", null, options);

            // See comment above about subscripts not being shifted
            if (base instanceof domTree.symbolNode) {
                supsub.children[0].style.marginLeft = -base.italic + "em";
            }

            supsub.children[0].style.marginRight = scriptspace;
            supsub.children[1].style.marginRight = scriptspace;
        }

        return makeSpan([getTypeOfGroup(group.value.base)],
            [base, supsub]);
    },

    genfrac: function(group, options, prev) {
        // Fractions are handled in the TeXbook on pages 444-445, rules 15(a-e).
        // Figure out what style this fraction should be in based on the
        // function used
        var fstyle = options.style;
        if (group.value.size === "display") {
            fstyle = Style.DISPLAY;
        } else if (group.value.size === "text") {
            fstyle = Style.TEXT;
        }

        var nstyle = fstyle.fracNum();
        var dstyle = fstyle.fracDen();

        var numer = buildGroup(group.value.numer, options.withStyle(nstyle));
        var numerreset = makeSpan([fstyle.reset(), nstyle.cls()], [numer]);

        var denom = buildGroup(group.value.denom, options.withStyle(dstyle));
        var denomreset = makeSpan([fstyle.reset(), dstyle.cls()], [denom]);

        var ruleWidth;
        if (group.value.hasBarLine) {
            ruleWidth = fontMetrics.metrics.defaultRuleThickness /
                options.style.sizeMultiplier;
        } else {
            ruleWidth = 0;
        }

        // Rule 15b
        var numShift;
        var clearance;
        var denomShift;
        if (fstyle.size === Style.DISPLAY.size) {
            numShift = fontMetrics.metrics.num1;
            if (ruleWidth > 0) {
                clearance = 3 * ruleWidth;
            } else {
                clearance = 7 * fontMetrics.metrics.defaultRuleThickness;
            }
            denomShift = fontMetrics.metrics.denom1;
        } else {
            if (ruleWidth > 0) {
                numShift = fontMetrics.metrics.num2;
                clearance = ruleWidth;
            } else {
                numShift = fontMetrics.metrics.num3;
                clearance = 3 * fontMetrics.metrics.defaultRuleThickness;
            }
            denomShift = fontMetrics.metrics.denom2;
        }

        var frac;
        if (ruleWidth === 0) {
            // Rule 15c
            var candiateClearance =
                (numShift - numer.depth) - (denom.height - denomShift);
            if (candiateClearance < clearance) {
                numShift += 0.5 * (clearance - candiateClearance);
                denomShift += 0.5 * (clearance - candiateClearance);
            }

            frac = buildCommon.makeVList([
                {type: "elem", elem: denomreset, shift: denomShift},
                {type: "elem", elem: numerreset, shift: -numShift}
            ], "individualShift", null, options);
        } else {
            // Rule 15d
            var axisHeight = fontMetrics.metrics.axisHeight;

            if ((numShift - numer.depth) - (axisHeight + 0.5 * ruleWidth)
                    < clearance) {
                numShift +=
                    clearance - ((numShift - numer.depth) -
                                 (axisHeight + 0.5 * ruleWidth));
            }

            if ((axisHeight - 0.5 * ruleWidth) - (denom.height - denomShift)
                    < clearance) {
                denomShift +=
                    clearance - ((axisHeight - 0.5 * ruleWidth) -
                                 (denom.height - denomShift));
            }

            var mid = makeSpan(
                [options.style.reset(), Style.TEXT.cls(), "frac-line"]);
            // Manually set the height of the line because its height is
            // created in CSS
            mid.height = ruleWidth;

            var midShift = -(axisHeight - 0.5 * ruleWidth);

            frac = buildCommon.makeVList([
                {type: "elem", elem: denomreset, shift: denomShift},
                {type: "elem", elem: mid,        shift: midShift},
                {type: "elem", elem: numerreset, shift: -numShift}
            ], "individualShift", null, options);
        }

        // Since we manually change the style sometimes (with \dfrac or \tfrac),
        // account for the possible size change here.
        frac.height *= fstyle.sizeMultiplier / options.style.sizeMultiplier;
        frac.depth *= fstyle.sizeMultiplier / options.style.sizeMultiplier;

        // Rule 15e
        var innerChildren = [makeSpan(["mfrac"], [frac])];

        var delimSize;
        if (fstyle.size === Style.DISPLAY.size) {
            delimSize = fontMetrics.metrics.delim1;
        } else {
            delimSize = fontMetrics.metrics.getDelim2(fstyle);
        }

        if (group.value.leftDelim != null) {
            innerChildren.unshift(
                delimiter.customSizedDelim(
                    group.value.leftDelim, delimSize, true,
                    options.withStyle(fstyle), group.mode)
            );
        }
        if (group.value.rightDelim != null) {
            innerChildren.push(
                delimiter.customSizedDelim(
                    group.value.rightDelim, delimSize, true,
                    options.withStyle(fstyle), group.mode)
            );
        }

        return makeSpan(
            ["minner", options.style.reset(), fstyle.cls()],
            innerChildren,
            options.getColor());
    },

    spacing: function(group, options, prev) {
        if (group.value === "\\ " || group.value === "\\space" ||
            group.value === " " || group.value === "~") {
            // Spaces are generated by adding an actual space. Each of these
            // things has an entry in the symbols table, so these will be turned
            // into appropriate outputs.
            return makeSpan(
                ["mord", "mspace"],
                [buildCommon.mathrm(group.value, group.mode)]
            );
        } else {
            // Other kinds of spaces are of arbitrary width. We use CSS to
            // generate these.
            var spacingClassMap = {
                "\\qquad": "qquad",
                "\\quad": "quad",
                "\\enspace": "enspace",
                "\\;": "thickspace",
                "\\:": "mediumspace",
                "\\,": "thinspace",
                "\\!": "negativethinspace"
            };

            return makeSpan(
                ["mord", "mspace", spacingClassMap[group.value]]);
        }
    },

    llap: function(group, options, prev) {
        var inner = makeSpan(
            ["inner"], [buildGroup(group.value.body, options.reset())]);
        var fix = makeSpan(["fix"], []);
        return makeSpan(
            ["llap", options.style.cls()], [inner, fix]);
    },

    rlap: function(group, options, prev) {
        var inner = makeSpan(
            ["inner"], [buildGroup(group.value.body, options.reset())]);
        var fix = makeSpan(["fix"], []);
        return makeSpan(
            ["rlap", options.style.cls()], [inner, fix]);
    },

    op: function(group, options, prev) {
        // Operators are handled in the TeXbook pg. 443-444, rule 13(a).
        var supGroup;
        var subGroup;
        var hasLimits = false;
        if (group.type === "supsub" ) {
            // If we have limits, supsub will pass us its group to handle. Pull
            // out the superscript and subscript and set the group to the op in
            // its base.
            supGroup = group.value.sup;
            subGroup = group.value.sub;
            group = group.value.base;
            hasLimits = true;
        }

        // Most operators have a large successor symbol, but these don't.
        var noSuccessor = [
            "\\smallint"
        ];

        var large = false;
        if (options.style.size === Style.DISPLAY.size &&
            group.value.symbol &&
            !utils.contains(noSuccessor, group.value.body)) {

            // Most symbol operators get larger in displaystyle (rule 13)
            large = true;
        }

        var base;
        var baseShift = 0;
        var slant = 0;
        if (group.value.symbol) {
            // If this is a symbol, create the symbol.
            var style = large ? "Size2-Regular" : "Size1-Regular";
            base = buildCommon.makeSymbol(
                group.value.body, style, "math", options.getColor(),
                ["op-symbol", large ? "large-op" : "small-op", "mop"]);

            // Shift the symbol so its center lies on the axis (rule 13). It
            // appears that our fonts have the centers of the symbols already
            // almost on the axis, so these numbers are very small. Note we
            // don't actually apply this here, but instead it is used either in
            // the vlist creation or separately when there are no limits.
            baseShift = (base.height - base.depth) / 2 -
                fontMetrics.metrics.axisHeight *
                options.style.sizeMultiplier;

            // The slant of the symbol is just its italic correction.
            slant = base.italic;
        } else {
            // Otherwise, this is a text operator. Build the text from the
            // operator's name.
            // TODO(emily): Add a space in the middle of some of these
            // operators, like \limsup
            var output = [];
            for (var i = 1; i < group.value.body.length; i++) {
                output.push(buildCommon.mathrm(group.value.body[i], group.mode));
            }
            base = makeSpan(["mop"], output, options.getColor());
        }

        if (hasLimits) {
            // IE 8 clips \int if it is in a display: inline-block. We wrap it
            // in a new span so it is an inline, and works.
            base = makeSpan([], [base]);

            var supmid, supKern, submid, subKern;
            // We manually have to handle the superscripts and subscripts. This,
            // aside from the kern calculations, is copied from supsub.
            if (supGroup) {
                var sup = buildGroup(
                    supGroup, options.withStyle(options.style.sup()));
                supmid = makeSpan(
                    [options.style.reset(), options.style.sup().cls()], [sup]);

                supKern = Math.max(
                    fontMetrics.metrics.bigOpSpacing1,
                    fontMetrics.metrics.bigOpSpacing3 - sup.depth);
            }

            if (subGroup) {
                var sub = buildGroup(
                    subGroup, options.withStyle(options.style.sub()));
                submid = makeSpan(
                    [options.style.reset(), options.style.sub().cls()],
                    [sub]);

                subKern = Math.max(
                    fontMetrics.metrics.bigOpSpacing2,
                    fontMetrics.metrics.bigOpSpacing4 - sub.height);
            }

            // Build the final group as a vlist of the possible subscript, base,
            // and possible superscript.
            var finalGroup, top, bottom;
            if (!supGroup) {
                top = base.height - baseShift;

                finalGroup = buildCommon.makeVList([
                    {type: "kern", size: fontMetrics.metrics.bigOpSpacing5},
                    {type: "elem", elem: submid},
                    {type: "kern", size: subKern},
                    {type: "elem", elem: base}
                ], "top", top, options);

                // Here, we shift the limits by the slant of the symbol. Note
                // that we are supposed to shift the limits by 1/2 of the slant,
                // but since we are centering the limits adding a full slant of
                // margin will shift by 1/2 that.
                finalGroup.children[0].style.marginLeft = -slant + "em";
            } else if (!subGroup) {
                bottom = base.depth + baseShift;

                finalGroup = buildCommon.makeVList([
                    {type: "elem", elem: base},
                    {type: "kern", size: supKern},
                    {type: "elem", elem: supmid},
                    {type: "kern", size: fontMetrics.metrics.bigOpSpacing5}
                ], "bottom", bottom, options);

                // See comment above about slants
                finalGroup.children[1].style.marginLeft = slant + "em";
            } else if (!supGroup && !subGroup) {
                // This case probably shouldn't occur (this would mean the
                // supsub was sending us a group with no superscript or
                // subscript) but be safe.
                return base;
            } else {
                bottom = fontMetrics.metrics.bigOpSpacing5 +
                    submid.height + submid.depth +
                    subKern +
                    base.depth + baseShift;

                finalGroup = buildCommon.makeVList([
                    {type: "kern", size: fontMetrics.metrics.bigOpSpacing5},
                    {type: "elem", elem: submid},
                    {type: "kern", size: subKern},
                    {type: "elem", elem: base},
                    {type: "kern", size: supKern},
                    {type: "elem", elem: supmid},
                    {type: "kern", size: fontMetrics.metrics.bigOpSpacing5}
                ], "bottom", bottom, options);

                // See comment above about slants
                finalGroup.children[0].style.marginLeft = -slant + "em";
                finalGroup.children[2].style.marginLeft = slant + "em";
            }

            return makeSpan(["mop", "op-limits"], [finalGroup]);
        } else {
            if (group.value.symbol) {
                base.style.top = baseShift + "em";
            }

            return base;
        }
    },

    katex: function(group, options, prev) {
        // The KaTeX logo. The offsets for the K and a were chosen to look
        // good, but the offsets for the T, E, and X were taken from the
        // definition of \TeX in TeX (see TeXbook pg. 356)
        var k = makeSpan(
            ["k"], [buildCommon.mathrm("K", group.mode)]);
        var a = makeSpan(
            ["a"], [buildCommon.mathrm("A", group.mode)]);

        a.height = (a.height + 0.2) * 0.75;
        a.depth = (a.height - 0.2) * 0.75;

        var t = makeSpan(
            ["t"], [buildCommon.mathrm("T", group.mode)]);
        var e = makeSpan(
            ["e"], [buildCommon.mathrm("E", group.mode)]);

        e.height = (e.height - 0.2155);
        e.depth = (e.depth + 0.2155);

        var x = makeSpan(
            ["x"], [buildCommon.mathrm("X", group.mode)]);

        return makeSpan(
            ["katex-logo"], [k, a, t, e, x], options.getColor());
    },

    overline: function(group, options, prev) {
        // Overlines are handled in the TeXbook pg 443, Rule 9.

        // Build the inner group in the cramped style.
        var innerGroup = buildGroup(group.value.body,
                options.withStyle(options.style.cramp()));

        var ruleWidth = fontMetrics.metrics.defaultRuleThickness /
            options.style.sizeMultiplier;

        // Create the line above the body
        var line = makeSpan(
            [options.style.reset(), Style.TEXT.cls(), "overline-line"]);
        line.height = ruleWidth;
        line.maxFontSize = 1.0;

        // Generate the vlist, with the appropriate kerns
        var vlist = buildCommon.makeVList([
            {type: "elem", elem: innerGroup},
            {type: "kern", size: 3 * ruleWidth},
            {type: "elem", elem: line},
            {type: "kern", size: ruleWidth}
        ], "firstBaseline", null, options);

        return makeSpan(["overline", "mord"], [vlist], options.getColor());
    },

    sqrt: function(group, options, prev) {
        // Square roots are handled in the TeXbook pg. 443, Rule 11.

        // First, we do the same steps as in overline to build the inner group
        // and line
        var inner = buildGroup(group.value.body,
                options.withStyle(options.style.cramp()));

        var ruleWidth = fontMetrics.metrics.defaultRuleThickness /
            options.style.sizeMultiplier;

        var line = makeSpan(
            [options.style.reset(), Style.TEXT.cls(), "sqrt-line"], [],
            options.getColor());
        line.height = ruleWidth;
        line.maxFontSize = 1.0;

        var phi = ruleWidth;
        if (options.style.id < Style.TEXT.id) {
            phi = fontMetrics.metrics.xHeight;
        }

        // Calculate the clearance between the body and line
        var lineClearance = ruleWidth + phi / 4;

        var innerHeight =
            (inner.height + inner.depth) * options.style.sizeMultiplier;
        var minDelimiterHeight = innerHeight + lineClearance + ruleWidth;

        // Create a \surd delimiter of the required minimum size
        var delim = makeSpan(["sqrt-sign"], [
            delimiter.customSizedDelim("\\surd", minDelimiterHeight,
                                       false, options, group.mode)],
                             options.getColor());

        var delimDepth = (delim.height + delim.depth) - ruleWidth;

        // Adjust the clearance based on the delimiter size
        if (delimDepth > inner.height + inner.depth + lineClearance) {
            lineClearance =
                (lineClearance + delimDepth - inner.height - inner.depth) / 2;
        }

        // Shift the delimiter so that its top lines up with the top of the line
        var delimShift = -(inner.height + lineClearance + ruleWidth) + delim.height;
        delim.style.top = delimShift + "em";
        delim.height -= delimShift;
        delim.depth += delimShift;

        // We add a special case here, because even when `inner` is empty, we
        // still get a line. So, we use a simple heuristic to decide if we
        // should omit the body entirely. (note this doesn't work for something
        // like `\sqrt{\rlap{x}}`, but if someone is doing that they deserve for
        // it not to work.
        var body;
        if (inner.height === 0 && inner.depth === 0) {
            body = makeSpan();
        } else {
            body = buildCommon.makeVList([
                {type: "elem", elem: inner},
                {type: "kern", size: lineClearance},
                {type: "elem", elem: line},
                {type: "kern", size: ruleWidth}
            ], "firstBaseline", null, options);
        }

        return makeSpan(["sqrt", "mord"], [delim, body]);
    },

    sizing: function(group, options, prev) {
        // Handle sizing operators like \Huge. Real TeX doesn't actually allow
        // these functions inside of math expressions, so we do some special
        // handling.
        var inner = buildExpression(group.value.value,
                options.withSize(group.value.size), prev);

        var span = makeSpan(["mord"],
            [makeSpan(["sizing", "reset-" + options.size, group.value.size,
                       options.style.cls()],
                      inner)]);

        // Calculate the correct maxFontSize manually
        var fontSize = sizingMultiplier[group.value.size];
        span.maxFontSize = fontSize * options.style.sizeMultiplier;

        return span;
    },

    styling: function(group, options, prev) {
        // Style changes are handled in the TeXbook on pg. 442, Rule 3.

        // Figure out what style we're changing to.
        var style = {
            "display": Style.DISPLAY,
            "text": Style.TEXT,
            "script": Style.SCRIPT,
            "scriptscript": Style.SCRIPTSCRIPT
        };

        var newStyle = style[group.value.style];

        // Build the inner expression in the new style.
        var inner = buildExpression(
            group.value.value, options.withStyle(newStyle), prev);

        return makeSpan([options.style.reset(), newStyle.cls()], inner);
    },

    delimsizing: function(group, options, prev) {
        var delim = group.value.value;

        if (delim === ".") {
            // Empty delimiters still count as elements, even though they don't
            // show anything.
            return makeSpan([groupToType[group.value.delimType]]);
        }

        // Use delimiter.sizedDelim to generate the delimiter.
        return makeSpan(
            [groupToType[group.value.delimType]],
            [delimiter.sizedDelim(
                delim, group.value.size, options, group.mode)]);
    },

    leftright: function(group, options, prev) {
        // Build the inner expression
        var inner = buildExpression(group.value.body, options.reset());

        var innerHeight = 0;
        var innerDepth = 0;

        // Calculate its height and depth
        for (var i = 0; i < inner.length; i++) {
            innerHeight = Math.max(inner[i].height, innerHeight);
            innerDepth = Math.max(inner[i].depth, innerDepth);
        }

        // The size of delimiters is the same, regardless of what style we are
        // in. Thus, to correctly calculate the size of delimiter we need around
        // a group, we scale down the inner size based on the size.
        innerHeight *= options.style.sizeMultiplier;
        innerDepth *= options.style.sizeMultiplier;

        var leftDelim;
        if (group.value.left === ".") {
            // Empty delimiters in \left and \right make null delimiter spaces.
            leftDelim = makeSpan(["nulldelimiter"]);
        } else {
            // Otherwise, use leftRightDelim to generate the correct sized
            // delimiter.
            leftDelim = delimiter.leftRightDelim(
                group.value.left, innerHeight, innerDepth, options,
                group.mode);
        }
        // Add it to the beginning of the expression
        inner.unshift(leftDelim);

        var rightDelim;
        // Same for the right delimiter
        if (group.value.right === ".") {
            rightDelim = makeSpan(["nulldelimiter"]);
        } else {
            rightDelim = delimiter.leftRightDelim(
                group.value.right, innerHeight, innerDepth, options,
                group.mode);
        }
        // Add it to the end of the expression.
        inner.push(rightDelim);

        return makeSpan(
            ["minner", options.style.cls()], inner, options.getColor());
    },

    rule: function(group, options, prev) {
        // Make an empty span for the rule
        var rule = makeSpan(["mord", "rule"], [], options.getColor());

        // Calculate the shift, width, and height of the rule, and account for units
        var shift = 0;
        if (group.value.shift) {
            shift = group.value.shift.number;
            if (group.value.shift.unit === "ex") {
                shift *= fontMetrics.metrics.xHeight;
            }
        }

        var width = group.value.width.number;
        if (group.value.width.unit === "ex") {
            width *= fontMetrics.metrics.xHeight;
        }

        var height = group.value.height.number;
        if (group.value.height.unit === "ex") {
            height *= fontMetrics.metrics.xHeight;
        }

        // The sizes of rules are absolute, so make it larger if we are in a
        // smaller style.
        shift /= options.style.sizeMultiplier;
        width /= options.style.sizeMultiplier;
        height /= options.style.sizeMultiplier;

        // Style the rule to the right size
        rule.style.borderRightWidth = width + "em";
        rule.style.borderTopWidth = height + "em";
        rule.style.bottom = shift + "em";

        // Record the height and width
        rule.width = width;
        rule.height = height + shift;
        rule.depth = -shift;

        return rule;
    },

    accent: function(group, options, prev) {
        // Accents are handled in the TeXbook pg. 443, rule 12.
        var base = group.value.base;

        var supsubGroup;
        if (group.type === "supsub") {
            // If our base is a character box, and we have superscripts and
            // subscripts, the supsub will defer to us. In particular, we want
            // to attach the superscripts and subscripts to the inner body (so
            // that the position of the superscripts and subscripts won't be
            // affected by the height of the accent). We accomplish this by
            // sticking the base of the accent into the base of the supsub, and
            // rendering that, while keeping track of where the accent is.

            // The supsub group is the group that was passed in
            var supsub = group;
            // The real accent group is the base of the supsub group
            group = supsub.value.base;
            // The character box is the base of the accent group
            base = group.value.base;
            // Stick the character box into the base of the supsub group
            supsub.value.base = base;

            // Rerender the supsub group with its new base, and store that
            // result.
            supsubGroup = buildGroup(
                supsub, options.reset(), prev);
        }

        // Build the base group
        var body = buildGroup(
            base, options.withStyle(options.style.cramp()));

        // Calculate the skew of the accent. This is based on the line "If the
        // nucleus is not a single character, let s = 0; otherwise set s to the
        // kern amount for the nucleus followed by the \skewchar of its font."
        // Note that our skew metrics are just the kern between each character
        // and the skewchar.
        var skew;
        if (isCharacterBox(base)) {
            // If the base is a character box, then we want the skew of the
            // innermost character. To do that, we find the innermost character:
            var baseChar = getBaseElem(base);
            // Then, we render its group to get the symbol inside it
            var baseGroup = buildGroup(
                baseChar, options.withStyle(options.style.cramp()));
            // Finally, we pull the skew off of the symbol.
            skew = baseGroup.skew;
            // Note that we now throw away baseGroup, because the layers we
            // removed with getBaseElem might contain things like \color which
            // we can't get rid of.
            // TODO(emily): Find a better way to get the skew
        } else {
            skew = 0;
        }

        // calculate the amount of space between the body and the accent
        var clearance = Math.min(body.height, fontMetrics.metrics.xHeight);

        // Build the accent
        var accent = buildCommon.makeSymbol(
            group.value.accent, "Main-Regular", "math", options.getColor());
        // Remove the italic correction of the accent, because it only serves to
        // shift the accent over to a place we don't want.
        accent.italic = 0;

        // The \vec character that the fonts use is a combining character, and
        // thus shows up much too far to the left. To account for this, we add a
        // specific class which shifts the accent over to where we want it.
        // TODO(emily): Fix this in a better way, like by changing the font
        var vecClass = group.value.accent === "\\vec" ? "accent-vec" : null;

        var accentBody = makeSpan(["accent-body", vecClass], [
            makeSpan([], [accent])]);

        accentBody = buildCommon.makeVList([
            {type: "elem", elem: body},
            {type: "kern", size: -clearance},
            {type: "elem", elem: accentBody}
        ], "firstBaseline", null, options);

        // Shift the accent over by the skew. Note we shift by twice the skew
        // because we are centering the accent, so by adding 2*skew to the left,
        // we shift it to the right by 1*skew.
        accentBody.children[1].style.marginLeft = 2 * skew + "em";

        var accentWrap = makeSpan(["mord", "accent"], [accentBody]);

        if (supsubGroup) {
            // Here, we replace the "base" child of the supsub with our newly
            // generated accent.
            supsubGroup.children[0] = accentWrap;

            // Since we don't rerun the height calculation after replacing the
            // accent, we manually recalculate height.
            supsubGroup.height = Math.max(accentWrap.height, supsubGroup.height);

            // Accents should always be ords, even when their innards are not.
            supsubGroup.classes[0] = "mord";

            return supsubGroup;
        } else {
            return accentWrap;
        }
    }
};

var sizingMultiplier = {
    size1: 0.5,
    size2: 0.7,
    size3: 0.8,
    size4: 0.9,
    size5: 1.0,
    size6: 1.2,
    size7: 1.44,
    size8: 1.73,
    size9: 2.07,
    size10: 2.49
};

/**
 * buildGroup is the function that takes a group and calls the correct groupType
 * function for it. It also handles the interaction of size and style changes
 * between parents and children.
 */
var buildGroup = function(group, options, prev) {
    if (!group) {
        return makeSpan();
    }

    if (groupTypes[group.type]) {
        // Call the groupTypes function
        var groupNode = groupTypes[group.type](group, options, prev);
        var multiplier;

        // If the style changed between the parent and the current group,
        // account for the size difference
        if (options.style !== options.parentStyle) {
            multiplier = options.style.sizeMultiplier /
                    options.parentStyle.sizeMultiplier;

            groupNode.height *= multiplier;
            groupNode.depth *= multiplier;
        }

        // If the size changed between the parent and the current group, account
        // for that size difference.
        if (options.size !== options.parentSize) {
            multiplier = sizingMultiplier[options.size] /
                    sizingMultiplier[options.parentSize];

            groupNode.height *= multiplier;
            groupNode.depth *= multiplier;
        }

        return groupNode;
    } else {
        throw new ParseError(
            "Got group of unknown type: '" + group.type + "'");
    }
};

/**
 * Take an entire parse tree, and build it into an appropriate set of nodes.
 */
var buildTree = function(tree) {
    // Setup the default options
    var options = new Options(Style.TEXT, "size5", "");

    // Build the expression contained in the tree
    var expression = buildExpression(tree, options);
    var body = makeSpan(["base", options.style.cls()], expression);

    // Add struts, which ensure that the top of the HTML element falls at the
    // height of the expression, and the bottom of the HTML element falls at the
    // depth of the expression.
    var topStrut = makeSpan(["strut"]);
    var bottomStrut = makeSpan(["strut", "bottom"]);

    topStrut.style.height = body.height + "em";
    bottomStrut.style.height = (body.height + body.depth) + "em";
    // We'd like to use `vertical-align: top` but in IE 9 this lowers the
    // baseline of the box to the bottom of this strut (instead staying in the
    // normal place) so we use an absolute value for vertical-align instead
    bottomStrut.style.verticalAlign = -body.depth + "em";

    // Wrap the struts and body together
    var katexNode = makeSpan(["katex"], [
        makeSpan(["katex-inner"], [topStrut, bottomStrut, body])
    ]);

    return katexNode;
};

module.exports = buildTree;

},{"./Options":4,"./ParseError":5,"./Style":7,"./buildCommon":8,"./delimiter":10,"./domTree":11,"./fontMetrics":12,"./utils":16}],10:[function(require,module,exports){
/**
 * This file deals with creating delimiters of various sizes. The TeXbook
 * discusses these routines on page 441-442, in the "Another subroutine sets box
 * x to a specified variable delimiter" paragraph.
 *
 * There are three main routines here. `makeSmallDelim` makes a delimiter in the
 * normal font, but in either text, script, or scriptscript style.
 * `makeLargeDelim` makes a delimiter in textstyle, but in one of the Size1,
 * Size2, Size3, or Size4 fonts. `makeStackedDelim` makes a delimiter out of
 * smaller pieces that are stacked on top of one another.
 *
 * The functions take a parameter `center`, which determines if the delimiter
 * should be centered around the axis.
 *
 * Then, there are three exposed functions. `sizedDelim` makes a delimiter in
 * one of the given sizes. This is used for things like `\bigl`.
 * `customSizedDelim` makes a delimiter with a given total height+depth. It is
 * called in places like `\sqrt`. `leftRightDelim` makes an appropriate
 * delimiter which surrounds an expression of a given height an depth. It is
 * used in `\left` and `\right`.
 */

var ParseError = require("./ParseError");
var Style = require("./Style");

var buildCommon = require("./buildCommon");
var fontMetrics = require("./fontMetrics");
var symbols = require("./symbols");
var utils = require("./utils");

var makeSpan = buildCommon.makeSpan;

/**
 * Get the metrics for a given symbol and font, after transformation (i.e.
 * after following replacement from symbols.js)
 */
var getMetrics = function(symbol, font) {
    if (symbols.math[symbol] && symbols.math[symbol].replace) {
        return fontMetrics.getCharacterMetrics(
            symbols.math[symbol].replace, font);
    } else {
        return fontMetrics.getCharacterMetrics(
            symbol, font);
    }
};

/**
 * Builds a symbol in the given font size (note size is an integer)
 */
var mathrmSize = function(value, size, mode) {
    return buildCommon.makeSymbol(value, "Size" + size + "-Regular", mode);
};

/**
 * Puts a delimiter span in a given style, and adds appropriate height, depth,
 * and maxFontSizes.
 */
var styleWrap = function(delim, toStyle, options) {
    var span = makeSpan(
        ["style-wrap", options.style.reset(), toStyle.cls()], [delim]);

    var multiplier = toStyle.sizeMultiplier / options.style.sizeMultiplier;

    span.height *= multiplier;
    span.depth *= multiplier;
    span.maxFontSize = toStyle.sizeMultiplier;

    return span;
};

/**
 * Makes a small delimiter. This is a delimiter that comes in the Main-Regular
 * font, but is restyled to either be in textstyle, scriptstyle, or
 * scriptscriptstyle.
 */
var makeSmallDelim = function(delim, style, center, options, mode) {
    var text = buildCommon.makeSymbol(delim, "Main-Regular", mode);

    var span = styleWrap(text, style, options);

    if (center) {
        var shift =
            (1 - options.style.sizeMultiplier / style.sizeMultiplier) *
            fontMetrics.metrics.axisHeight;

        span.style.top = shift + "em";
        span.height -= shift;
        span.depth += shift;
    }

    return span;
};

/**
 * Makes a large delimiter. This is a delimiter that comes in the Size1, Size2,
 * Size3, or Size4 fonts. It is always rendered in textstyle.
 */
var makeLargeDelim = function(delim, size, center, options, mode) {
    var inner = mathrmSize(delim, size, mode);

    var span = styleWrap(
        makeSpan(["delimsizing", "size" + size],
                 [inner], options.getColor()),
        Style.TEXT, options);

    if (center) {
        var shift = (1 - options.style.sizeMultiplier) *
            fontMetrics.metrics.axisHeight;

        span.style.top = shift + "em";
        span.height -= shift;
        span.depth += shift;
    }

    return span;
};

/**
 * Make an inner span with the given offset and in the given font. This is used
 * in `makeStackedDelim` to make the stacking pieces for the delimiter.
 */
var makeInner = function(symbol, font, mode) {
    var sizeClass;
    // Apply the correct CSS class to choose the right font.
    if (font === "Size1-Regular") {
        sizeClass = "delim-size1";
    } else if (font === "Size4-Regular") {
        sizeClass = "delim-size4";
    }

    var inner = makeSpan(
        ["delimsizinginner", sizeClass],
        [makeSpan([], [buildCommon.makeSymbol(symbol, font, mode)])]);

    // Since this will be passed into `makeVList` in the end, wrap the element
    // in the appropriate tag that VList uses.
    return {type: "elem", elem: inner};
};

/**
 * Make a stacked delimiter out of a given delimiter, with the total height at
 * least `heightTotal`. This routine is mentioned on page 442 of the TeXbook.
 */
var makeStackedDelim = function(delim, heightTotal, center, options, mode) {
    // There are four parts, the top, an optional middle, a repeated part, and a
    // bottom.
    var top, middle, repeat, bottom;
    top = repeat = bottom = delim;
    middle = null;
    // Also keep track of what font the delimiters are in
    var font = "Size1-Regular";

    // We set the parts and font based on the symbol. Note that we use
    // '\u23d0' instead of '|' and '\u2016' instead of '\\|' for the
    // repeats of the arrows
    if (delim === "\\uparrow") {
        repeat = bottom = "\u23d0";
    } else if (delim === "\\Uparrow") {
        repeat = bottom = "\u2016";
    } else if (delim === "\\downarrow") {
        top = repeat = "\u23d0";
    } else if (delim === "\\Downarrow") {
        top = repeat = "\u2016";
    } else if (delim === "\\updownarrow") {
        top = "\\uparrow";
        repeat = "\u23d0";
        bottom = "\\downarrow";
    } else if (delim === "\\Updownarrow") {
        top = "\\Uparrow";
        repeat = "\u2016";
        bottom = "\\Downarrow";
    } else if (delim === "[" || delim === "\\lbrack") {
        top = "\u23a1";
        repeat = "\u23a2";
        bottom = "\u23a3";
        font = "Size4-Regular";
    } else if (delim === "]" || delim === "\\rbrack") {
        top = "\u23a4";
        repeat = "\u23a5";
        bottom = "\u23a6";
        font = "Size4-Regular";
    } else if (delim === "\\lfloor") {
        repeat = top = "\u23a2";
        bottom = "\u23a3";
        font = "Size4-Regular";
    } else if (delim === "\\lceil") {
        top = "\u23a1";
        repeat = bottom = "\u23a2";
        font = "Size4-Regular";
    } else if (delim === "\\rfloor") {
        repeat = top = "\u23a5";
        bottom = "\u23a6";
        font = "Size4-Regular";
    } else if (delim === "\\rceil") {
        top = "\u23a4";
        repeat = bottom = "\u23a5";
        font = "Size4-Regular";
    } else if (delim === "(") {
        top = "\u239b";
        repeat = "\u239c";
        bottom = "\u239d";
        font = "Size4-Regular";
    } else if (delim === ")") {
        top = "\u239e";
        repeat = "\u239f";
        bottom = "\u23a0";
        font = "Size4-Regular";
    } else if (delim === "\\{" || delim === "\\lbrace") {
        top = "\u23a7";
        middle = "\u23a8";
        bottom = "\u23a9";
        repeat = "\u23aa";
        font = "Size4-Regular";
    } else if (delim === "\\}" || delim === "\\rbrace") {
        top = "\u23ab";
        middle = "\u23ac";
        bottom = "\u23ad";
        repeat = "\u23aa";
        font = "Size4-Regular";
    } else if (delim === "\\surd") {
        top = "\ue001";
        bottom = "\u23b7";
        repeat = "\ue000";
        font = "Size4-Regular";
    }

    // Get the metrics of the four sections
    var topMetrics = getMetrics(top, font);
    var topHeightTotal = topMetrics.height + topMetrics.depth;
    var repeatMetrics = getMetrics(repeat, font);
    var repeatHeightTotal = repeatMetrics.height + repeatMetrics.depth;
    var bottomMetrics = getMetrics(bottom, font);
    var bottomHeightTotal = bottomMetrics.height + bottomMetrics.depth;
    var middleMetrics, middleHeightTotal;
    if (middle !== null) {
        middleMetrics = getMetrics(middle, font);
        middleHeightTotal = middleMetrics.height + middleMetrics.depth;
    }

    // Calcuate the real height that the delimiter will have. It is at least the
    // size of the top, bottom, and optional middle combined.
    var realHeightTotal = topHeightTotal + bottomHeightTotal;
    if (middle !== null) {
        realHeightTotal += middleHeightTotal;
    }

    // Then add repeated pieces until we reach the specified height.
    while (realHeightTotal < heightTotal) {
        realHeightTotal += repeatHeightTotal;
        if (middle !== null) {
            // If there is a middle section, we need an equal number of pieces
            // on the top and bottom.
            realHeightTotal += repeatHeightTotal;
        }
    }

    // The center of the delimiter is placed at the center of the axis. Note
    // that in this context, "center" means that the delimiter should be
    // centered around the axis in the current style, while normally it is
    // centered around the axis in textstyle.
    var axisHeight = fontMetrics.metrics.axisHeight;
    if (center) {
        axisHeight *= options.style.sizeMultiplier;
    }
    // Calculate the depth
    var depth = realHeightTotal / 2 - axisHeight;

    // Now, we start building the pieces that will go into the vlist

    // Keep a list of the inner pieces
    var inners = [];

    // Add the bottom symbol
    inners.push(makeInner(bottom, font, mode));

    var i;
    if (middle === null) {
        // Calculate the number of repeated symbols we need
        var repeatHeight = realHeightTotal - topHeightTotal - bottomHeightTotal;
        var symbolCount = Math.ceil(repeatHeight / repeatHeightTotal);

        // Add that many symbols
        for (i = 0; i < symbolCount; i++) {
            inners.push(makeInner(repeat, font, mode));
        }
    } else {
        // When there is a middle bit, we need the middle part and two repeated
        // sections

        // Calculate the number of symbols needed for the top and bottom
        // repeated parts
        var topRepeatHeight =
            realHeightTotal / 2 - topHeightTotal - middleHeightTotal / 2;
        var topSymbolCount = Math.ceil(topRepeatHeight / repeatHeightTotal);

        var bottomRepeatHeight =
            realHeightTotal / 2 - topHeightTotal - middleHeightTotal / 2;
        var bottomSymbolCount =
            Math.ceil(bottomRepeatHeight / repeatHeightTotal);

        // Add the top repeated part
        for (i = 0; i < topSymbolCount; i++) {
            inners.push(makeInner(repeat, font, mode));
        }

        // Add the middle piece
        inners.push(makeInner(middle, font, mode));

        // Add the bottom repeated part
        for (i = 0; i < bottomSymbolCount; i++) {
            inners.push(makeInner(repeat, font, mode));
        }
    }

    // Add the top symbol
    inners.push(makeInner(top, font, mode));

    // Finally, build the vlist
    var inner = buildCommon.makeVList(inners, "bottom", depth, options);

    return styleWrap(
        makeSpan(["delimsizing", "mult"], [inner], options.getColor()),
        Style.TEXT, options);
};

// There are three kinds of delimiters, delimiters that stack when they become
// too large
var stackLargeDelimiters = [
    "(", ")", "[", "\\lbrack", "]", "\\rbrack",
    "\\{", "\\lbrace", "\\}", "\\rbrace",
    "\\lfloor", "\\rfloor", "\\lceil", "\\rceil",
    "\\surd"
];

// delimiters that always stack
var stackAlwaysDelimiters = [
    "\\uparrow", "\\downarrow", "\\updownarrow",
    "\\Uparrow", "\\Downarrow", "\\Updownarrow",
    "|", "\\|", "\\vert", "\\Vert"
];

// and delimiters that never stack
var stackNeverDelimiters = [
    "<", ">", "\\langle", "\\rangle", "/", "\\backslash"
];

// Metrics of the different sizes. Found by looking at TeX's output of
// $\bigl| // \Bigl| \biggl| \Biggl| \showlists$
// Used to create stacked delimiters of appropriate sizes in makeSizedDelim.
var sizeToMaxHeight = [0, 1.2, 1.8, 2.4, 3.0];

/**
 * Used to create a delimiter of a specific size, where `size` is 1, 2, 3, or 4.
 */
var makeSizedDelim = function(delim, size, options, mode) {
    // < and > turn into \langle and \rangle in delimiters
    if (delim === "<") {
        delim = "\\langle";
    } else if (delim === ">") {
        delim = "\\rangle";
    }

    // Sized delimiters are never centered.
    if (utils.contains(stackLargeDelimiters, delim) ||
        utils.contains(stackNeverDelimiters, delim)) {
        return makeLargeDelim(delim, size, false, options, mode);
    } else if (utils.contains(stackAlwaysDelimiters, delim)) {
        return makeStackedDelim(
            delim, sizeToMaxHeight[size], false, options, mode);
    } else {
        throw new ParseError("Illegal delimiter: '" + delim + "'");
    }
};

/**
 * There are three different sequences of delimiter sizes that the delimiters
 * follow depending on the kind of delimiter. This is used when creating custom
 * sized delimiters to decide whether to create a small, large, or stacked
 * delimiter.
 *
 * In real TeX, these sequences aren't explicitly defined, but are instead
 * defined inside the font metrics. Since there are only three sequences that
 * are possible for the delimiters that TeX defines, it is easier to just encode
 * them explicitly here.
 */

// Delimiters that never stack try small delimiters and large delimiters only
var stackNeverDelimiterSequence = [
    {type: "small", style: Style.SCRIPTSCRIPT},
    {type: "small", style: Style.SCRIPT},
    {type: "small", style: Style.TEXT},
    {type: "large", size: 1},
    {type: "large", size: 2},
    {type: "large", size: 3},
    {type: "large", size: 4}
];

// Delimiters that always stack try the small delimiters first, then stack
var stackAlwaysDelimiterSequence = [
    {type: "small", style: Style.SCRIPTSCRIPT},
    {type: "small", style: Style.SCRIPT},
    {type: "small", style: Style.TEXT},
    {type: "stack"}
];

// Delimiters that stack when large try the small and then large delimiters, and
// stack afterwards
var stackLargeDelimiterSequence = [
    {type: "small", style: Style.SCRIPTSCRIPT},
    {type: "small", style: Style.SCRIPT},
    {type: "small", style: Style.TEXT},
    {type: "large", size: 1},
    {type: "large", size: 2},
    {type: "large", size: 3},
    {type: "large", size: 4},
    {type: "stack"}
];

/**
 * Get the font used in a delimiter based on what kind of delimiter it is.
 */
var delimTypeToFont = function(type) {
    if (type.type === "small") {
        return "Main-Regular";
    } else if (type.type === "large") {
        return "Size" + type.size + "-Regular";
    } else if (type.type === "stack") {
        return "Size4-Regular";
    }
};

/**
 * Traverse a sequence of types of delimiters to decide what kind of delimiter
 * should be used to create a delimiter of the given height+depth.
 */
var traverseSequence = function(delim, height, sequence, options) {
    // Here, we choose the index we should start at in the sequences. In smaller
    // sizes (which correspond to larger numbers in style.size) we start earlier
    // in the sequence. Thus, scriptscript starts at index 3-3=0, script starts
    // at index 3-2=1, text starts at 3-1=2, and display starts at min(2,3-0)=2
    var start = Math.min(2, 3 - options.style.size);
    for (var i = start; i < sequence.length; i++) {
        if (sequence[i].type === "stack") {
            // This is always the last delimiter, so we just break the loop now.
            break;
        }

        var metrics = getMetrics(delim, delimTypeToFont(sequence[i]));
        var heightDepth = metrics.height + metrics.depth;

        // Small delimiters are scaled down versions of the same font, so we
        // account for the style change size.

        if (sequence[i].type === "small") {
            heightDepth *= sequence[i].style.sizeMultiplier;
        }

        // Check if the delimiter at this size works for the given height.
        if (heightDepth > height) {
            return sequence[i];
        }
    }

    // If we reached the end of the sequence, return the last sequence element.
    return sequence[sequence.length - 1];
};

/**
 * Make a delimiter of a given height+depth, with optional centering. Here, we
 * traverse the sequences, and create a delimiter that the sequence tells us to.
 */
var makeCustomSizedDelim = function(delim, height, center, options, mode) {
    if (delim === "<") {
        delim = "\\langle";
    } else if (delim === ">") {
        delim = "\\rangle";
    }

    // Decide what sequence to use
    var sequence;
    if (utils.contains(stackNeverDelimiters, delim)) {
        sequence = stackNeverDelimiterSequence;
    } else if (utils.contains(stackLargeDelimiters, delim)) {
        sequence = stackLargeDelimiterSequence;
    } else {
        sequence = stackAlwaysDelimiterSequence;
    }

    // Look through the sequence
    var delimType = traverseSequence(delim, height, sequence, options);

    // Depending on the sequence element we decided on, call the appropriate
    // function.
    if (delimType.type === "small") {
        return makeSmallDelim(delim, delimType.style, center, options, mode);
    } else if (delimType.type === "large") {
        return makeLargeDelim(delim, delimType.size, center, options, mode);
    } else if (delimType.type === "stack") {
        return makeStackedDelim(delim, height, center, options, mode);
    }
};

/**
 * Make a delimiter for use with `\left` and `\right`, given a height and depth
 * of an expression that the delimiters surround.
 */
var makeLeftRightDelim = function(delim, height, depth, options, mode) {
    // We always center \left/\right delimiters, so the axis is always shifted
    var axisHeight =
        fontMetrics.metrics.axisHeight * options.style.sizeMultiplier;

    // Taken from TeX source, tex.web, function make_left_right
    var delimiterFactor = 901;
    var delimiterExtend = 5.0 / fontMetrics.metrics.ptPerEm;

    var maxDistFromAxis = Math.max(
        height - axisHeight, depth + axisHeight);

    var totalHeight = Math.max(
        // In real TeX, calculations are done using integral values which are
        // 65536 per pt, or 655360 per em. So, the division here truncates in
        // TeX but doesn't here, producing different results. If we wanted to
        // exactly match TeX's calculation, we could do
        //   Math.floor(655360 * maxDistFromAxis / 500) *
        //    delimiterFactor / 655360
        // (To see the difference, compare
        //    x^{x^{\left(\rule{0.1em}{0.68em}\right)}}
        // in TeX and KaTeX)
        maxDistFromAxis / 500 * delimiterFactor,
        2 * maxDistFromAxis - delimiterExtend);

    // Finally, we defer to `makeCustomSizedDelim` with our calculated total
    // height
    return makeCustomSizedDelim(delim, totalHeight, true, options, mode);
};

module.exports = {
    sizedDelim: makeSizedDelim,
    customSizedDelim: makeCustomSizedDelim,
    leftRightDelim: makeLeftRightDelim
};

},{"./ParseError":5,"./Style":7,"./buildCommon":8,"./fontMetrics":12,"./symbols":15,"./utils":16}],11:[function(require,module,exports){
/**
 * These objects store the data about the DOM nodes we create, as well as some
 * extra data. They can then be transformed into real DOM nodes with the toNode
 * function or HTML markup using toMarkup. They are useful for both storing
 * extra properties on the nodes, as well as providing a way to easily work
 * with the DOM.
 */

var utils = require("./utils");

/**
 * Create an HTML className based on a list of classes. In addition to joining
 * with spaces, we also remove null or empty classes.
 */
var createClass = function(classes) {
    classes = classes.slice();
    for (var i = classes.length - 1; i >= 0; i--) {
        if (!classes[i]) {
            classes.splice(i, 1);
        }
    }

    return classes.join(" ");
};

/**
 * This node represents a span node, with a className, a list of children, and
 * an inline style. It also contains information about its height, depth, and
 * maxFontSize.
 */
function span(classes, children, height, depth, maxFontSize, style) {
    this.classes = classes || [];
    this.children = children || [];
    this.height = height || 0;
    this.depth = depth || 0;
    this.maxFontSize = maxFontSize || 0;
    this.style = style || {};
}

/**
 * Convert the span into an HTML node
 */
span.prototype.toNode = function() {
    var span = document.createElement("span");

    // Apply the class
    span.className = createClass(this.classes);

    // Apply inline styles
    for (var style in this.style) {
        if (this.style.hasOwnProperty(style)) {
            span.style[style] = this.style[style];
        }
    }

    // Append the children, also as HTML nodes
    for (var i = 0; i < this.children.length; i++) {
        span.appendChild(this.children[i].toNode());
    }

    return span;
};

/**
 * Convert the span into an HTML markup string
 */
span.prototype.toMarkup = function() {
    var markup = "<span";

    // Add the class
    if (this.classes.length) {
        markup += " class=\"";
        markup += utils.escape(createClass(this.classes));
        markup += "\"";
    }

    var styles = "";

    // Add the styles, after hyphenation
    for (var style in this.style) {
        if (this.style.hasOwnProperty(style)) {
            styles += utils.hyphenate(style) + ":" + this.style[style] + ";";
        }
    }

    if (styles) {
        markup += " style=\"" + utils.escape(styles) + "\"";
    }

    markup += ">";

    // Add the markup of the children, also as markup
    for (var i = 0; i < this.children.length; i++) {
        markup += this.children[i].toMarkup();
    }

    markup += "</span>";

    return markup;
};

/**
 * This node represents a document fragment, which contains elements, but when
 * placed into the DOM doesn't have any representation itself. Thus, it only
 * contains children and doesn't have any HTML properties. It also keeps track
 * of a height, depth, and maxFontSize.
 */
function documentFragment(children, height, depth, maxFontSize) {
    this.children = children || [];
    this.height = height || 0;
    this.depth = depth || 0;
    this.maxFontSize = maxFontSize || 0;
}

/**
 * Convert the fragment into a node
 */
documentFragment.prototype.toNode = function() {
    // Create a fragment
    var frag = document.createDocumentFragment();

    // Append the children
    for (var i = 0; i < this.children.length; i++) {
        frag.appendChild(this.children[i].toNode());
    }

    return frag;
};

/**
 * Convert the fragment into HTML markup
 */
documentFragment.prototype.toMarkup = function() {
    var markup = "";

    // Simply concatenate the markup for the children together
    for (var i = 0; i < this.children.length; i++) {
        markup += this.children[i].toMarkup();
    }

    return markup;
};

/**
 * A symbol node contains information about a single symbol. It either renders
 * to a single text node, or a span with a single text node in it, depending on
 * whether it has CSS classes, styles, or needs italic correction.
 */
function symbolNode(value, height, depth, italic, skew, classes, style) {
    this.value = value || "";
    this.height = height || 0;
    this.depth = depth || 0;
    this.italic = italic || 0;
    this.skew = skew || 0;
    this.classes = classes || [];
    this.style = style || {};
    this.maxFontSize = 0;
}

/**
 * Creates a text node or span from a symbol node. Note that a span is only
 * created if it is needed.
 */
symbolNode.prototype.toNode = function() {
    var node = document.createTextNode(this.value);
    var span = null;

    if (this.italic > 0) {
        span = document.createElement("span");
        span.style.marginRight = this.italic + "em";
    }

    if (this.classes.length > 0) {
        span = span || document.createElement("span");
        span.className = createClass(this.classes);
    }

    for (var style in this.style) {
        if (this.style.hasOwnProperty(style)) {
            span = span || document.createElement("span");
            span.style[style] = this.style[style];
        }
    }

    if (span) {
        span.appendChild(node);
        return span;
    } else {
        return node;
    }
};

/**
 * Creates markup for a symbol node.
 */
symbolNode.prototype.toMarkup = function() {
    // TODO(alpert): More duplication than I'd like from
    // span.prototype.toMarkup and symbolNode.prototype.toNode...
    var needsSpan = false;

    var markup = "<span";

    if (this.classes.length) {
        needsSpan = true;
        markup += " class=\"";
        markup += utils.escape(createClass(this.classes));
        markup += "\"";
    }

    var styles = "";

    if (this.italic > 0) {
        styles += "margin-right:" + this.italic + "em;";
    }
    for (var style in this.style) {
        if (this.style.hasOwnProperty(style)) {
            styles += utils.hyphenate(style) + ":" + this.style[style] + ";";
        }
    }

    if (styles) {
        needsSpan = true;
        markup += " style=\"" + utils.escape(styles) + "\"";
    }

    var escaped = utils.escape(this.value);
    if (needsSpan) {
        markup += ">";
        markup += escaped;
        markup += "</span>";
        return markup;
    } else {
        return escaped;
    }
};

module.exports = {
    span: span,
    documentFragment: documentFragment,
    symbolNode: symbolNode
};

},{"./utils":16}],12:[function(require,module,exports){
/* jshint unused:false */

var Style = require("./Style");

/**
 * This file contains metrics regarding fonts and individual symbols. The sigma
 * and xi variables, as well as the metricMap map contain data extracted from
 * TeX, TeX font metrics, and the TTF files. These data are then exposed via the
 * `metrics` variable and the getCharacterMetrics function.
 */

// These font metrics are extracted from TeX by using
// \font\a=cmmi10
// \showthe\fontdimenX\a
// where X is the corresponding variable number. These correspond to the font
// parameters of the symbol fonts. In TeX, there are actually three sets of
// dimensions, one for each of textstyle, scriptstyle, and scriptscriptstyle,
// but we only use the textstyle ones, and scale certain dimensions accordingly.
// See the TeXbook, page 441.
var sigma1 = 0.025;
var sigma2 = 0;
var sigma3 = 0;
var sigma4 = 0;
var sigma5 = 0.431;
var sigma6 = 1;
var sigma7 = 0;
var sigma8 = 0.677;
var sigma9 = 0.394;
var sigma10 = 0.444;
var sigma11 = 0.686;
var sigma12 = 0.345;
var sigma13 = 0.413;
var sigma14 = 0.363;
var sigma15 = 0.289;
var sigma16 = 0.150;
var sigma17 = 0.247;
var sigma18 = 0.386;
var sigma19 = 0.050;
var sigma20 = 2.390;
var sigma21 = 1.01;
var sigma21Script = 0.81;
var sigma21ScriptScript = 0.71;
var sigma22 = 0.250;

// These font metrics are extracted from TeX by using
// \font\a=cmex10
// \showthe\fontdimenX\a
// where X is the corresponding variable number. These correspond to the font
// parameters of the extension fonts (family 3). See the TeXbook, page 441.
var xi1 = 0;
var xi2 = 0;
var xi3 = 0;
var xi4 = 0;
var xi5 = 0.431;
var xi6 = 1;
var xi7 = 0;
var xi8 = 0.04;
var xi9 = 0.111;
var xi10 = 0.166;
var xi11 = 0.2;
var xi12 = 0.6;
var xi13 = 0.1;

// This value determines how large a pt is, for metrics which are defined in
// terms of pts.
// This value is also used in katex.less; if you change it make sure the values
// match.
var ptPerEm = 10.0;

/**
 * This is just a mapping from common names to real metrics
 */
var metrics = {
    xHeight: sigma5,
    quad: sigma6,
    num1: sigma8,
    num2: sigma9,
    num3: sigma10,
    denom1: sigma11,
    denom2: sigma12,
    sup1: sigma13,
    sup2: sigma14,
    sup3: sigma15,
    sub1: sigma16,
    sub2: sigma17,
    supDrop: sigma18,
    subDrop: sigma19,
    axisHeight: sigma22,
    defaultRuleThickness: xi8,
    bigOpSpacing1: xi9,
    bigOpSpacing2: xi10,
    bigOpSpacing3: xi11,
    bigOpSpacing4: xi12,
    bigOpSpacing5: xi13,
    ptPerEm: ptPerEm,

    // TODO(alpert): Missing parallel structure here. We should probably add
    // style-specific metrics for all of these.
    delim1: sigma20,
    getDelim2: function(style) {
        if (style.size === Style.TEXT.size) {
            return sigma21;
        } else if (style.size === Style.SCRIPT.size) {
            return sigma21Script;
        } else if (style.size === Style.SCRIPTSCRIPT.size) {
            return sigma21ScriptScript;
        }
        throw new Error("Unexpected style size: " + style.size);
    }
};

// This map contains a mapping from font name and character code to character
// metrics, including height, depth, italic correction, and skew (kern from the
// character to the corresponding \skewchar)
// This map is generated via `make metrics`. It should not be changed manually.
var metricMap = {"AMS-Regular":{"10003":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"10016":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"1008":{"depth":0.0,"height":0.43056,"italic":0.04028,"skew":0.0},"107":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"10731":{"depth":0.11111,"height":0.69224,"italic":0.0,"skew":0.0},"10846":{"depth":0.19444,"height":0.75583,"italic":0.0,"skew":0.0},"10877":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"10878":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"10885":{"depth":0.25583,"height":0.75583,"italic":0.0,"skew":0.0},"10886":{"depth":0.25583,"height":0.75583,"italic":0.0,"skew":0.0},"10887":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"10888":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"10889":{"depth":0.26167,"height":0.75726,"italic":0.0,"skew":0.0},"10890":{"depth":0.26167,"height":0.75726,"italic":0.0,"skew":0.0},"10891":{"depth":0.48256,"height":0.98256,"italic":0.0,"skew":0.0},"10892":{"depth":0.48256,"height":0.98256,"italic":0.0,"skew":0.0},"10901":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"10902":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"10933":{"depth":0.25142,"height":0.75726,"italic":0.0,"skew":0.0},"10934":{"depth":0.25142,"height":0.75726,"italic":0.0,"skew":0.0},"10935":{"depth":0.26167,"height":0.75726,"italic":0.0,"skew":0.0},"10936":{"depth":0.26167,"height":0.75726,"italic":0.0,"skew":0.0},"10937":{"depth":0.26167,"height":0.75726,"italic":0.0,"skew":0.0},"10938":{"depth":0.26167,"height":0.75726,"italic":0.0,"skew":0.0},"10949":{"depth":0.25583,"height":0.75583,"italic":0.0,"skew":0.0},"10950":{"depth":0.25583,"height":0.75583,"italic":0.0,"skew":0.0},"10955":{"depth":0.28481,"height":0.79383,"italic":0.0,"skew":0.0},"10956":{"depth":0.28481,"height":0.79383,"italic":0.0,"skew":0.0},"165":{"depth":0.0,"height":0.675,"italic":0.025,"skew":0.0},"174":{"depth":0.15559,"height":0.69224,"italic":0.0,"skew":0.0},"240":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"295":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"57350":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0},"57351":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0},"57352":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0},"57353":{"depth":0.0,"height":0.43056,"italic":0.04028,"skew":0.0},"57356":{"depth":0.25142,"height":0.75726,"italic":0.0,"skew":0.0},"57357":{"depth":0.25142,"height":0.75726,"italic":0.0,"skew":0.0},"57358":{"depth":0.41951,"height":0.91951,"italic":0.0,"skew":0.0},"57359":{"depth":0.30274,"height":0.79383,"italic":0.0,"skew":0.0},"57360":{"depth":0.30274,"height":0.79383,"italic":0.0,"skew":0.0},"57361":{"depth":0.41951,"height":0.91951,"italic":0.0,"skew":0.0},"57366":{"depth":0.25142,"height":0.75726,"italic":0.0,"skew":0.0},"57367":{"depth":0.25142,"height":0.75726,"italic":0.0,"skew":0.0},"57368":{"depth":0.25142,"height":0.75726,"italic":0.0,"skew":0.0},"57369":{"depth":0.25142,"height":0.75726,"italic":0.0,"skew":0.0},"57370":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"57371":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"65":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"66":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"67":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"68":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"69":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"70":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"71":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"710":{"depth":0.0,"height":0.825,"italic":0.0,"skew":0.0},"72":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"73":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"732":{"depth":0.0,"height":0.9,"italic":0.0,"skew":0.0},"74":{"depth":0.16667,"height":0.68889,"italic":0.0,"skew":0.0},"75":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"76":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"77":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"770":{"depth":0.0,"height":0.825,"italic":0.0,"skew":0.0},"771":{"depth":0.0,"height":0.9,"italic":0.0,"skew":0.0},"78":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"79":{"depth":0.16667,"height":0.68889,"italic":0.0,"skew":0.0},"80":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"81":{"depth":0.16667,"height":0.68889,"italic":0.0,"skew":0.0},"82":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8245":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"83":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"84":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8463":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8487":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8498":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"85":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8502":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8503":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8504":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8513":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8592":{"depth":-0.03598,"height":0.46402,"italic":0.0,"skew":0.0},"8594":{"depth":-0.03598,"height":0.46402,"italic":0.0,"skew":0.0},"86":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8602":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8603":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8606":{"depth":0.01354,"height":0.52239,"italic":0.0,"skew":0.0},"8608":{"depth":0.01354,"height":0.52239,"italic":0.0,"skew":0.0},"8610":{"depth":0.01354,"height":0.52239,"italic":0.0,"skew":0.0},"8611":{"depth":0.01354,"height":0.52239,"italic":0.0,"skew":0.0},"8619":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"8620":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"8621":{"depth":-0.13313,"height":0.37788,"italic":0.0,"skew":0.0},"8622":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8624":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8625":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8630":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"8631":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"8634":{"depth":0.08198,"height":0.58198,"italic":0.0,"skew":0.0},"8635":{"depth":0.08198,"height":0.58198,"italic":0.0,"skew":0.0},"8638":{"depth":0.19444,"height":0.69224,"italic":0.0,"skew":0.0},"8639":{"depth":0.19444,"height":0.69224,"italic":0.0,"skew":0.0},"8642":{"depth":0.19444,"height":0.69224,"italic":0.0,"skew":0.0},"8643":{"depth":0.19444,"height":0.69224,"italic":0.0,"skew":0.0},"8644":{"depth":0.1808,"height":0.675,"italic":0.0,"skew":0.0},"8646":{"depth":0.1808,"height":0.675,"italic":0.0,"skew":0.0},"8647":{"depth":0.1808,"height":0.675,"italic":0.0,"skew":0.0},"8648":{"depth":0.19444,"height":0.69224,"italic":0.0,"skew":0.0},"8649":{"depth":0.1808,"height":0.675,"italic":0.0,"skew":0.0},"8650":{"depth":0.19444,"height":0.69224,"italic":0.0,"skew":0.0},"8651":{"depth":0.01354,"height":0.52239,"italic":0.0,"skew":0.0},"8652":{"depth":0.01354,"height":0.52239,"italic":0.0,"skew":0.0},"8653":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8654":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8655":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8666":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"8667":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"8669":{"depth":-0.13313,"height":0.37788,"italic":0.0,"skew":0.0},"87":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8705":{"depth":0.0,"height":0.825,"italic":0.0,"skew":0.0},"8708":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8709":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0},"8717":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"8722":{"depth":-0.03598,"height":0.46402,"italic":0.0,"skew":0.0},"8724":{"depth":0.08198,"height":0.69224,"italic":0.0,"skew":0.0},"8726":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0},"8733":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8736":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8737":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8738":{"depth":0.03517,"height":0.52239,"italic":0.0,"skew":0.0},"8739":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0},"8740":{"depth":0.25142,"height":0.74111,"italic":0.0,"skew":0.0},"8741":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0},"8742":{"depth":0.25142,"height":0.74111,"italic":0.0,"skew":0.0},"8756":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8757":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8764":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8765":{"depth":-0.13313,"height":0.37788,"italic":0.0,"skew":0.0},"8769":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8770":{"depth":-0.03625,"height":0.46375,"italic":0.0,"skew":0.0},"8774":{"depth":0.30274,"height":0.79383,"italic":0.0,"skew":0.0},"8776":{"depth":-0.01688,"height":0.48312,"italic":0.0,"skew":0.0},"8778":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0},"8782":{"depth":0.06062,"height":0.54986,"italic":0.0,"skew":0.0},"8783":{"depth":0.06062,"height":0.54986,"italic":0.0,"skew":0.0},"8785":{"depth":0.08198,"height":0.58198,"italic":0.0,"skew":0.0},"8786":{"depth":0.08198,"height":0.58198,"italic":0.0,"skew":0.0},"8787":{"depth":0.08198,"height":0.58198,"italic":0.0,"skew":0.0},"8790":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8791":{"depth":0.22958,"height":0.72958,"italic":0.0,"skew":0.0},"8796":{"depth":0.08198,"height":0.91667,"italic":0.0,"skew":0.0},"88":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8806":{"depth":0.25583,"height":0.75583,"italic":0.0,"skew":0.0},"8807":{"depth":0.25583,"height":0.75583,"italic":0.0,"skew":0.0},"8808":{"depth":0.25142,"height":0.75726,"italic":0.0,"skew":0.0},"8809":{"depth":0.25142,"height":0.75726,"italic":0.0,"skew":0.0},"8812":{"depth":0.25583,"height":0.75583,"italic":0.0,"skew":0.0},"8814":{"depth":0.20576,"height":0.70576,"italic":0.0,"skew":0.0},"8815":{"depth":0.20576,"height":0.70576,"italic":0.0,"skew":0.0},"8816":{"depth":0.30274,"height":0.79383,"italic":0.0,"skew":0.0},"8817":{"depth":0.30274,"height":0.79383,"italic":0.0,"skew":0.0},"8818":{"depth":0.22958,"height":0.72958,"italic":0.0,"skew":0.0},"8819":{"depth":0.22958,"height":0.72958,"italic":0.0,"skew":0.0},"8822":{"depth":0.1808,"height":0.675,"italic":0.0,"skew":0.0},"8823":{"depth":0.1808,"height":0.675,"italic":0.0,"skew":0.0},"8828":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"8829":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"8830":{"depth":0.22958,"height":0.72958,"italic":0.0,"skew":0.0},"8831":{"depth":0.22958,"height":0.72958,"italic":0.0,"skew":0.0},"8832":{"depth":0.20576,"height":0.70576,"italic":0.0,"skew":0.0},"8833":{"depth":0.20576,"height":0.70576,"italic":0.0,"skew":0.0},"8840":{"depth":0.30274,"height":0.79383,"italic":0.0,"skew":0.0},"8841":{"depth":0.30274,"height":0.79383,"italic":0.0,"skew":0.0},"8842":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"8843":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"8847":{"depth":0.03517,"height":0.54986,"italic":0.0,"skew":0.0},"8848":{"depth":0.03517,"height":0.54986,"italic":0.0,"skew":0.0},"8858":{"depth":0.08198,"height":0.58198,"italic":0.0,"skew":0.0},"8859":{"depth":0.08198,"height":0.58198,"italic":0.0,"skew":0.0},"8861":{"depth":0.08198,"height":0.58198,"italic":0.0,"skew":0.0},"8862":{"depth":0.0,"height":0.675,"italic":0.0,"skew":0.0},"8863":{"depth":0.0,"height":0.675,"italic":0.0,"skew":0.0},"8864":{"depth":0.0,"height":0.675,"italic":0.0,"skew":0.0},"8865":{"depth":0.0,"height":0.675,"italic":0.0,"skew":0.0},"8872":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8873":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8874":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8876":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8877":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8878":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8879":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8882":{"depth":0.03517,"height":0.54986,"italic":0.0,"skew":0.0},"8883":{"depth":0.03517,"height":0.54986,"italic":0.0,"skew":0.0},"8884":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"8885":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"8888":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"8890":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.0},"8891":{"depth":0.19444,"height":0.69224,"italic":0.0,"skew":0.0},"8892":{"depth":0.19444,"height":0.69224,"italic":0.0,"skew":0.0},"89":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8901":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"8903":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0},"8905":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0},"8906":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0},"8907":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8908":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8909":{"depth":-0.03598,"height":0.46402,"italic":0.0,"skew":0.0},"8910":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"8911":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"8912":{"depth":0.03517,"height":0.54986,"italic":0.0,"skew":0.0},"8913":{"depth":0.03517,"height":0.54986,"italic":0.0,"skew":0.0},"8914":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"8915":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"8916":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8918":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"8919":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"8920":{"depth":0.03517,"height":0.54986,"italic":0.0,"skew":0.0},"8921":{"depth":0.03517,"height":0.54986,"italic":0.0,"skew":0.0},"8922":{"depth":0.38569,"height":0.88569,"italic":0.0,"skew":0.0},"8923":{"depth":0.38569,"height":0.88569,"italic":0.0,"skew":0.0},"8926":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"8927":{"depth":0.13667,"height":0.63667,"italic":0.0,"skew":0.0},"8928":{"depth":0.30274,"height":0.79383,"italic":0.0,"skew":0.0},"8929":{"depth":0.30274,"height":0.79383,"italic":0.0,"skew":0.0},"8934":{"depth":0.23222,"height":0.74111,"italic":0.0,"skew":0.0},"8935":{"depth":0.23222,"height":0.74111,"italic":0.0,"skew":0.0},"8936":{"depth":0.23222,"height":0.74111,"italic":0.0,"skew":0.0},"8937":{"depth":0.23222,"height":0.74111,"italic":0.0,"skew":0.0},"8938":{"depth":0.20576,"height":0.70576,"italic":0.0,"skew":0.0},"8939":{"depth":0.20576,"height":0.70576,"italic":0.0,"skew":0.0},"8940":{"depth":0.30274,"height":0.79383,"italic":0.0,"skew":0.0},"8941":{"depth":0.30274,"height":0.79383,"italic":0.0,"skew":0.0},"8994":{"depth":0.19444,"height":0.69224,"italic":0.0,"skew":0.0},"8995":{"depth":0.19444,"height":0.69224,"italic":0.0,"skew":0.0},"90":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"9416":{"depth":0.15559,"height":0.69224,"italic":0.0,"skew":0.0},"9484":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"9488":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"9492":{"depth":0.0,"height":0.37788,"italic":0.0,"skew":0.0},"9496":{"depth":0.0,"height":0.37788,"italic":0.0,"skew":0.0},"9585":{"depth":0.19444,"height":0.68889,"italic":0.0,"skew":0.0},"9586":{"depth":0.19444,"height":0.74111,"italic":0.0,"skew":0.0},"9632":{"depth":0.0,"height":0.675,"italic":0.0,"skew":0.0},"9633":{"depth":0.0,"height":0.675,"italic":0.0,"skew":0.0},"9650":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"9651":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"9654":{"depth":0.03517,"height":0.54986,"italic":0.0,"skew":0.0},"9660":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"9661":{"depth":0.0,"height":0.54986,"italic":0.0,"skew":0.0},"9664":{"depth":0.03517,"height":0.54986,"italic":0.0,"skew":0.0},"9674":{"depth":0.11111,"height":0.69224,"italic":0.0,"skew":0.0},"9733":{"depth":0.19444,"height":0.69224,"italic":0.0,"skew":0.0},"989":{"depth":0.08167,"height":0.58167,"italic":0.0,"skew":0.0}},"Main-Bold":{"100":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"101":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"102":{"depth":0.0,"height":0.69444,"italic":0.10903,"skew":0.0},"10216":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"10217":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"103":{"depth":0.19444,"height":0.44444,"italic":0.01597,"skew":0.0},"104":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"105":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"106":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"107":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"108":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"10815":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"109":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"10927":{"depth":0.19667,"height":0.69667,"italic":0.0,"skew":0.0},"10928":{"depth":0.19667,"height":0.69667,"italic":0.0,"skew":0.0},"110":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"111":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"112":{"depth":0.19444,"height":0.44444,"italic":0.0,"skew":0.0},"113":{"depth":0.19444,"height":0.44444,"italic":0.0,"skew":0.0},"114":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"115":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"116":{"depth":0.0,"height":0.63492,"italic":0.0,"skew":0.0},"117":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"118":{"depth":0.0,"height":0.44444,"italic":0.01597,"skew":0.0},"119":{"depth":0.0,"height":0.44444,"italic":0.01597,"skew":0.0},"120":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"121":{"depth":0.19444,"height":0.44444,"italic":0.01597,"skew":0.0},"122":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"123":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"124":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"125":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"126":{"depth":0.35,"height":0.34444,"italic":0.0,"skew":0.0},"168":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"172":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"175":{"depth":0.0,"height":0.59611,"italic":0.0,"skew":0.0},"176":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"177":{"depth":0.13333,"height":0.63333,"italic":0.0,"skew":0.0},"180":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"215":{"depth":0.13333,"height":0.63333,"italic":0.0,"skew":0.0},"247":{"depth":0.13333,"height":0.63333,"italic":0.0,"skew":0.0},"305":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"33":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"34":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"35":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"36":{"depth":0.05556,"height":0.75,"italic":0.0,"skew":0.0},"37":{"depth":0.05556,"height":0.75,"italic":0.0,"skew":0.0},"38":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"39":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"40":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"41":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"42":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"43":{"depth":0.13333,"height":0.63333,"italic":0.0,"skew":0.0},"44":{"depth":0.19444,"height":0.15556,"italic":0.0,"skew":0.0},"45":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"46":{"depth":0.0,"height":0.15556,"italic":0.0,"skew":0.0},"47":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"48":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"49":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"50":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"51":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"52":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"53":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"54":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"55":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"56":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"567":{"depth":0.19444,"height":0.44444,"italic":0.0,"skew":0.0},"57":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"58":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"59":{"depth":0.19444,"height":0.44444,"italic":0.0,"skew":0.0},"60":{"depth":0.08556,"height":0.58556,"italic":0.0,"skew":0.0},"61":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"62":{"depth":0.08556,"height":0.58556,"italic":0.0,"skew":0.0},"63":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"64":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"65":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"66":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"67":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"68":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"69":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"70":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"71":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"710":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"711":{"depth":0.0,"height":0.63194,"italic":0.0,"skew":0.0},"713":{"depth":0.0,"height":0.59611,"italic":0.0,"skew":0.0},"714":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"715":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"72":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"728":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"729":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"73":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"730":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"732":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"74":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"75":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"76":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"768":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"769":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"77":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"770":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"771":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"772":{"depth":0.0,"height":0.59611,"italic":0.0,"skew":0.0},"774":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"775":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"776":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"778":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"779":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"78":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"780":{"depth":0.0,"height":0.63194,"italic":0.0,"skew":0.0},"79":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"80":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"81":{"depth":0.19444,"height":0.68611,"italic":0.0,"skew":0.0},"82":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"8211":{"depth":0.0,"height":0.44444,"italic":0.03194,"skew":0.0},"8212":{"depth":0.0,"height":0.44444,"italic":0.03194,"skew":0.0},"8216":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8217":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8220":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8221":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8224":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8225":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"824":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8242":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"83":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"84":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"8407":{"depth":0.0,"height":0.72444,"italic":0.15486,"skew":0.0},"8463":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8465":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8467":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8472":{"depth":0.19444,"height":0.44444,"italic":0.0,"skew":0.0},"8476":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"85":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"8501":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8592":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"8593":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8594":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"8595":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8596":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"8597":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8598":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8599":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"86":{"depth":0.0,"height":0.68611,"italic":0.01597,"skew":0.0},"8600":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8601":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8636":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"8637":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"8640":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"8641":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"8656":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"8657":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8658":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"8659":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8660":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"8661":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"87":{"depth":0.0,"height":0.68611,"italic":0.01597,"skew":0.0},"8704":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8706":{"depth":0.0,"height":0.69444,"italic":0.06389,"skew":0.0},"8707":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8709":{"depth":0.05556,"height":0.75,"italic":0.0,"skew":0.0},"8711":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"8712":{"depth":0.08556,"height":0.58556,"italic":0.0,"skew":0.0},"8715":{"depth":0.08556,"height":0.58556,"italic":0.0,"skew":0.0},"8722":{"depth":0.13333,"height":0.63333,"italic":0.0,"skew":0.0},"8723":{"depth":0.13333,"height":0.63333,"italic":0.0,"skew":0.0},"8725":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8726":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8727":{"depth":-0.02778,"height":0.47222,"italic":0.0,"skew":0.0},"8728":{"depth":-0.02639,"height":0.47361,"italic":0.0,"skew":0.0},"8729":{"depth":-0.02639,"height":0.47361,"italic":0.0,"skew":0.0},"8730":{"depth":0.18,"height":0.82,"italic":0.0,"skew":0.0},"8733":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"8734":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"8736":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8739":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8741":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8743":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8744":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8745":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8746":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8747":{"depth":0.19444,"height":0.69444,"italic":0.12778,"skew":0.0},"8764":{"depth":-0.10889,"height":0.39111,"italic":0.0,"skew":0.0},"8768":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8771":{"depth":0.00222,"height":0.50222,"italic":0.0,"skew":0.0},"8776":{"depth":0.02444,"height":0.52444,"italic":0.0,"skew":0.0},"8781":{"depth":0.00222,"height":0.50222,"italic":0.0,"skew":0.0},"88":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"8801":{"depth":0.00222,"height":0.50222,"italic":0.0,"skew":0.0},"8804":{"depth":0.19667,"height":0.69667,"italic":0.0,"skew":0.0},"8805":{"depth":0.19667,"height":0.69667,"italic":0.0,"skew":0.0},"8810":{"depth":0.08556,"height":0.58556,"italic":0.0,"skew":0.0},"8811":{"depth":0.08556,"height":0.58556,"italic":0.0,"skew":0.0},"8826":{"depth":0.08556,"height":0.58556,"italic":0.0,"skew":0.0},"8827":{"depth":0.08556,"height":0.58556,"italic":0.0,"skew":0.0},"8834":{"depth":0.08556,"height":0.58556,"italic":0.0,"skew":0.0},"8835":{"depth":0.08556,"height":0.58556,"italic":0.0,"skew":0.0},"8838":{"depth":0.19667,"height":0.69667,"italic":0.0,"skew":0.0},"8839":{"depth":0.19667,"height":0.69667,"italic":0.0,"skew":0.0},"8846":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8849":{"depth":0.19667,"height":0.69667,"italic":0.0,"skew":0.0},"8850":{"depth":0.19667,"height":0.69667,"italic":0.0,"skew":0.0},"8851":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8852":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8853":{"depth":0.13333,"height":0.63333,"italic":0.0,"skew":0.0},"8854":{"depth":0.13333,"height":0.63333,"italic":0.0,"skew":0.0},"8855":{"depth":0.13333,"height":0.63333,"italic":0.0,"skew":0.0},"8856":{"depth":0.13333,"height":0.63333,"italic":0.0,"skew":0.0},"8857":{"depth":0.13333,"height":0.63333,"italic":0.0,"skew":0.0},"8866":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8867":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8868":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8869":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"89":{"depth":0.0,"height":0.68611,"italic":0.02875,"skew":0.0},"8900":{"depth":-0.02639,"height":0.47361,"italic":0.0,"skew":0.0},"8901":{"depth":-0.02639,"height":0.47361,"italic":0.0,"skew":0.0},"8902":{"depth":-0.02778,"height":0.47222,"italic":0.0,"skew":0.0},"8968":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8969":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8970":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8971":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8994":{"depth":-0.13889,"height":0.36111,"italic":0.0,"skew":0.0},"8995":{"depth":-0.13889,"height":0.36111,"italic":0.0,"skew":0.0},"90":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"91":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"915":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"916":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"92":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"920":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"923":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"926":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"928":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"93":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"931":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"933":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"934":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"936":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"937":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"94":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"95":{"depth":0.31,"height":0.13444,"italic":0.03194,"skew":0.0},"96":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"9651":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"9657":{"depth":-0.02778,"height":0.47222,"italic":0.0,"skew":0.0},"9661":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"9667":{"depth":-0.02778,"height":0.47222,"italic":0.0,"skew":0.0},"97":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"9711":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"98":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"9824":{"depth":0.12963,"height":0.69444,"italic":0.0,"skew":0.0},"9825":{"depth":0.12963,"height":0.69444,"italic":0.0,"skew":0.0},"9826":{"depth":0.12963,"height":0.69444,"italic":0.0,"skew":0.0},"9827":{"depth":0.12963,"height":0.69444,"italic":0.0,"skew":0.0},"9837":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"9838":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"9839":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"99":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0}},"Main-Italic":{"100":{"depth":0.0,"height":0.69444,"italic":0.10333,"skew":0.0},"101":{"depth":0.0,"height":0.43056,"italic":0.07514,"skew":0.0},"102":{"depth":0.19444,"height":0.69444,"italic":0.21194,"skew":0.0},"103":{"depth":0.19444,"height":0.43056,"italic":0.08847,"skew":0.0},"104":{"depth":0.0,"height":0.69444,"italic":0.07671,"skew":0.0},"105":{"depth":0.0,"height":0.65536,"italic":0.1019,"skew":0.0},"106":{"depth":0.19444,"height":0.65536,"italic":0.14467,"skew":0.0},"107":{"depth":0.0,"height":0.69444,"italic":0.10764,"skew":0.0},"108":{"depth":0.0,"height":0.69444,"italic":0.10333,"skew":0.0},"109":{"depth":0.0,"height":0.43056,"italic":0.07671,"skew":0.0},"110":{"depth":0.0,"height":0.43056,"italic":0.07671,"skew":0.0},"111":{"depth":0.0,"height":0.43056,"italic":0.06312,"skew":0.0},"112":{"depth":0.19444,"height":0.43056,"italic":0.06312,"skew":0.0},"113":{"depth":0.19444,"height":0.43056,"italic":0.08847,"skew":0.0},"114":{"depth":0.0,"height":0.43056,"italic":0.10764,"skew":0.0},"115":{"depth":0.0,"height":0.43056,"italic":0.08208,"skew":0.0},"116":{"depth":0.0,"height":0.61508,"italic":0.09486,"skew":0.0},"117":{"depth":0.0,"height":0.43056,"italic":0.07671,"skew":0.0},"118":{"depth":0.0,"height":0.43056,"italic":0.10764,"skew":0.0},"119":{"depth":0.0,"height":0.43056,"italic":0.10764,"skew":0.0},"120":{"depth":0.0,"height":0.43056,"italic":0.12042,"skew":0.0},"121":{"depth":0.19444,"height":0.43056,"italic":0.08847,"skew":0.0},"122":{"depth":0.0,"height":0.43056,"italic":0.12292,"skew":0.0},"126":{"depth":0.35,"height":0.31786,"italic":0.11585,"skew":0.0},"163":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"305":{"depth":0.0,"height":0.43056,"italic":0.07671,"skew":0.0},"33":{"depth":0.0,"height":0.69444,"italic":0.12417,"skew":0.0},"34":{"depth":0.0,"height":0.69444,"italic":0.06961,"skew":0.0},"35":{"depth":0.19444,"height":0.69444,"italic":0.06616,"skew":0.0},"37":{"depth":0.05556,"height":0.75,"italic":0.13639,"skew":0.0},"38":{"depth":0.0,"height":0.69444,"italic":0.09694,"skew":0.0},"39":{"depth":0.0,"height":0.69444,"italic":0.12417,"skew":0.0},"40":{"depth":0.25,"height":0.75,"italic":0.16194,"skew":0.0},"41":{"depth":0.25,"height":0.75,"italic":0.03694,"skew":0.0},"42":{"depth":0.0,"height":0.75,"italic":0.14917,"skew":0.0},"43":{"depth":0.05667,"height":0.56167,"italic":0.03694,"skew":0.0},"44":{"depth":0.19444,"height":0.10556,"italic":0.0,"skew":0.0},"45":{"depth":0.0,"height":0.43056,"italic":0.02826,"skew":0.0},"46":{"depth":0.0,"height":0.10556,"italic":0.0,"skew":0.0},"47":{"depth":0.25,"height":0.75,"italic":0.16194,"skew":0.0},"48":{"depth":0.0,"height":0.64444,"italic":0.13556,"skew":0.0},"49":{"depth":0.0,"height":0.64444,"italic":0.13556,"skew":0.0},"50":{"depth":0.0,"height":0.64444,"italic":0.13556,"skew":0.0},"51":{"depth":0.0,"height":0.64444,"italic":0.13556,"skew":0.0},"52":{"depth":0.19444,"height":0.64444,"italic":0.13556,"skew":0.0},"53":{"depth":0.0,"height":0.64444,"italic":0.13556,"skew":0.0},"54":{"depth":0.0,"height":0.64444,"italic":0.13556,"skew":0.0},"55":{"depth":0.19444,"height":0.64444,"italic":0.13556,"skew":0.0},"56":{"depth":0.0,"height":0.64444,"italic":0.13556,"skew":0.0},"567":{"depth":0.19444,"height":0.43056,"italic":0.03736,"skew":0.0},"57":{"depth":0.0,"height":0.64444,"italic":0.13556,"skew":0.0},"58":{"depth":0.0,"height":0.43056,"italic":0.0582,"skew":0.0},"59":{"depth":0.19444,"height":0.43056,"italic":0.0582,"skew":0.0},"61":{"depth":-0.13313,"height":0.36687,"italic":0.06616,"skew":0.0},"63":{"depth":0.0,"height":0.69444,"italic":0.1225,"skew":0.0},"64":{"depth":0.0,"height":0.69444,"italic":0.09597,"skew":0.0},"65":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"66":{"depth":0.0,"height":0.68333,"italic":0.10257,"skew":0.0},"67":{"depth":0.0,"height":0.68333,"italic":0.14528,"skew":0.0},"68":{"depth":0.0,"height":0.68333,"italic":0.09403,"skew":0.0},"69":{"depth":0.0,"height":0.68333,"italic":0.12028,"skew":0.0},"70":{"depth":0.0,"height":0.68333,"italic":0.13305,"skew":0.0},"71":{"depth":0.0,"height":0.68333,"italic":0.08722,"skew":0.0},"72":{"depth":0.0,"height":0.68333,"italic":0.16389,"skew":0.0},"73":{"depth":0.0,"height":0.68333,"italic":0.15806,"skew":0.0},"74":{"depth":0.0,"height":0.68333,"italic":0.14028,"skew":0.0},"75":{"depth":0.0,"height":0.68333,"italic":0.14528,"skew":0.0},"76":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"768":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"769":{"depth":0.0,"height":0.69444,"italic":0.09694,"skew":0.0},"77":{"depth":0.0,"height":0.68333,"italic":0.16389,"skew":0.0},"770":{"depth":0.0,"height":0.69444,"italic":0.06646,"skew":0.0},"771":{"depth":0.0,"height":0.66786,"italic":0.11585,"skew":0.0},"772":{"depth":0.0,"height":0.56167,"italic":0.10333,"skew":0.0},"774":{"depth":0.0,"height":0.69444,"italic":0.10806,"skew":0.0},"775":{"depth":0.0,"height":0.66786,"italic":0.11752,"skew":0.0},"776":{"depth":0.0,"height":0.66786,"italic":0.10474,"skew":0.0},"778":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"779":{"depth":0.0,"height":0.69444,"italic":0.1225,"skew":0.0},"78":{"depth":0.0,"height":0.68333,"italic":0.16389,"skew":0.0},"780":{"depth":0.0,"height":0.62847,"italic":0.08295,"skew":0.0},"79":{"depth":0.0,"height":0.68333,"italic":0.09403,"skew":0.0},"80":{"depth":0.0,"height":0.68333,"italic":0.10257,"skew":0.0},"81":{"depth":0.19444,"height":0.68333,"italic":0.09403,"skew":0.0},"82":{"depth":0.0,"height":0.68333,"italic":0.03868,"skew":0.0},"8211":{"depth":0.0,"height":0.43056,"italic":0.09208,"skew":0.0},"8212":{"depth":0.0,"height":0.43056,"italic":0.09208,"skew":0.0},"8216":{"depth":0.0,"height":0.69444,"italic":0.12417,"skew":0.0},"8217":{"depth":0.0,"height":0.69444,"italic":0.12417,"skew":0.0},"8220":{"depth":0.0,"height":0.69444,"italic":0.1685,"skew":0.0},"8221":{"depth":0.0,"height":0.69444,"italic":0.06961,"skew":0.0},"83":{"depth":0.0,"height":0.68333,"italic":0.11972,"skew":0.0},"84":{"depth":0.0,"height":0.68333,"italic":0.13305,"skew":0.0},"8463":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"85":{"depth":0.0,"height":0.68333,"italic":0.16389,"skew":0.0},"86":{"depth":0.0,"height":0.68333,"italic":0.18361,"skew":0.0},"87":{"depth":0.0,"height":0.68333,"italic":0.18361,"skew":0.0},"88":{"depth":0.0,"height":0.68333,"italic":0.15806,"skew":0.0},"89":{"depth":0.0,"height":0.68333,"italic":0.19383,"skew":0.0},"90":{"depth":0.0,"height":0.68333,"italic":0.14528,"skew":0.0},"91":{"depth":0.25,"height":0.75,"italic":0.1875,"skew":0.0},"915":{"depth":0.0,"height":0.68333,"italic":0.13305,"skew":0.0},"916":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"920":{"depth":0.0,"height":0.68333,"italic":0.09403,"skew":0.0},"923":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"926":{"depth":0.0,"height":0.68333,"italic":0.15294,"skew":0.0},"928":{"depth":0.0,"height":0.68333,"italic":0.16389,"skew":0.0},"93":{"depth":0.25,"height":0.75,"italic":0.10528,"skew":0.0},"931":{"depth":0.0,"height":0.68333,"italic":0.12028,"skew":0.0},"933":{"depth":0.0,"height":0.68333,"italic":0.11111,"skew":0.0},"934":{"depth":0.0,"height":0.68333,"italic":0.05986,"skew":0.0},"936":{"depth":0.0,"height":0.68333,"italic":0.11111,"skew":0.0},"937":{"depth":0.0,"height":0.68333,"italic":0.10257,"skew":0.0},"94":{"depth":0.0,"height":0.69444,"italic":0.06646,"skew":0.0},"95":{"depth":0.31,"height":0.12056,"italic":0.09208,"skew":0.0},"97":{"depth":0.0,"height":0.43056,"italic":0.07671,"skew":0.0},"98":{"depth":0.0,"height":0.69444,"italic":0.06312,"skew":0.0},"99":{"depth":0.0,"height":0.43056,"italic":0.05653,"skew":0.0}},"Main-Regular":{"32":{"depth":-0.0,"height":0.0,"italic":0,"skew":0},"160":{"depth":-0.0,"height":0.0,"italic":0,"skew":0},"8230":{"depth":-0.0,"height":0.12,"italic":0,"skew":0},"8773":{"depth":-0.022,"height":0.589,"italic":0,"skew":0},"8800":{"depth":0.215,"height":0.716,"italic":0,"skew":0},"8942":{"depth":0.03,"height":0.9,"italic":0,"skew":0},"8943":{"depth":-0.19,"height":0.31,"italic":0,"skew":0},"8945":{"depth":-0.1,"height":0.82,"italic":0,"skew":0},"100":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"101":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"102":{"depth":0.0,"height":0.69444,"italic":0.07778,"skew":0.0},"10216":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"10217":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"103":{"depth":0.19444,"height":0.43056,"italic":0.01389,"skew":0.0},"104":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"105":{"depth":0.0,"height":0.66786,"italic":0.0,"skew":0.0},"106":{"depth":0.19444,"height":0.66786,"italic":0.0,"skew":0.0},"107":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"108":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"10815":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"109":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"10927":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"10928":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"110":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"111":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"112":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.0},"113":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.0},"114":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"115":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"116":{"depth":0.0,"height":0.61508,"italic":0.0,"skew":0.0},"117":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"118":{"depth":0.0,"height":0.43056,"italic":0.01389,"skew":0.0},"119":{"depth":0.0,"height":0.43056,"italic":0.01389,"skew":0.0},"120":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"121":{"depth":0.19444,"height":0.43056,"italic":0.01389,"skew":0.0},"122":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"123":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"124":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"125":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"126":{"depth":0.35,"height":0.31786,"italic":0.0,"skew":0.0},"168":{"depth":0.0,"height":0.66786,"italic":0.0,"skew":0.0},"172":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"175":{"depth":0.0,"height":0.56778,"italic":0.0,"skew":0.0},"176":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"177":{"depth":0.08333,"height":0.58333,"italic":0.0,"skew":0.0},"180":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"215":{"depth":0.08333,"height":0.58333,"italic":0.0,"skew":0.0},"247":{"depth":0.08333,"height":0.58333,"italic":0.0,"skew":0.0},"305":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"33":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"34":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"35":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"36":{"depth":0.05556,"height":0.75,"italic":0.0,"skew":0.0},"37":{"depth":0.05556,"height":0.75,"italic":0.0,"skew":0.0},"38":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"39":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"40":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"41":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"42":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"43":{"depth":0.08333,"height":0.58333,"italic":0.0,"skew":0.0},"44":{"depth":0.19444,"height":0.10556,"italic":0.0,"skew":0.0},"45":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"46":{"depth":0.0,"height":0.10556,"italic":0.0,"skew":0.0},"47":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"48":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"49":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"50":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"51":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"52":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"53":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"54":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"55":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"56":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"567":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.0},"57":{"depth":0.0,"height":0.64444,"italic":0.0,"skew":0.0},"58":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"59":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.0},"60":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"61":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"62":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"63":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"64":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"65":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"66":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"67":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"68":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"69":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"70":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"71":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"710":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"711":{"depth":0.0,"height":0.62847,"italic":0.0,"skew":0.0},"713":{"depth":0.0,"height":0.56778,"italic":0.0,"skew":0.0},"714":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"715":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"72":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"728":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"729":{"depth":0.0,"height":0.66786,"italic":0.0,"skew":0.0},"73":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"730":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"732":{"depth":0.0,"height":0.66786,"italic":0.0,"skew":0.0},"74":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"75":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"76":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"768":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"769":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"77":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"770":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"771":{"depth":0.0,"height":0.66786,"italic":0.0,"skew":0.0},"772":{"depth":0.0,"height":0.56778,"italic":0.0,"skew":0.0},"774":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"775":{"depth":0.0,"height":0.66786,"italic":0.0,"skew":0.0},"776":{"depth":0.0,"height":0.66786,"italic":0.0,"skew":0.0},"778":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"779":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"78":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"780":{"depth":0.0,"height":0.62847,"italic":0.0,"skew":0.0},"79":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"80":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"81":{"depth":0.19444,"height":0.68333,"italic":0.0,"skew":0.0},"82":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"8211":{"depth":0.0,"height":0.43056,"italic":0.02778,"skew":0.0},"8212":{"depth":0.0,"height":0.43056,"italic":0.02778,"skew":0.0},"8216":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8217":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8220":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8221":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8224":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8225":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"824":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8242":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"83":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"84":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"8407":{"depth":0.0,"height":0.71444,"italic":0.15382,"skew":0.0},"8463":{"depth":0.0,"height":0.68889,"italic":0.0,"skew":0.0},"8465":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8467":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.11111},"8472":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.11111},"8476":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"85":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"8501":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8592":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8593":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8594":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8595":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8596":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8597":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8598":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8599":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"86":{"depth":0.0,"height":0.68333,"italic":0.01389,"skew":0.0},"8600":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8601":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8636":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8637":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8640":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8641":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8656":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8657":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8658":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8659":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8660":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8661":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"87":{"depth":0.0,"height":0.68333,"italic":0.01389,"skew":0.0},"8704":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8706":{"depth":0.0,"height":0.69444,"italic":0.05556,"skew":0.08334},"8707":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8709":{"depth":0.05556,"height":0.75,"italic":0.0,"skew":0.0},"8711":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"8712":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"8715":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"8722":{"depth":0.08333,"height":0.58333,"italic":0.0,"skew":0.0},"8723":{"depth":0.08333,"height":0.58333,"italic":0.0,"skew":0.0},"8725":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8726":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8727":{"depth":-0.03472,"height":0.46528,"italic":0.0,"skew":0.0},"8728":{"depth":-0.05555,"height":0.44445,"italic":0.0,"skew":0.0},"8729":{"depth":-0.05555,"height":0.44445,"italic":0.0,"skew":0.0},"8730":{"depth":0.2,"height":0.8,"italic":0.0,"skew":0.0},"8733":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"8734":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"8736":{"depth":0.0,"height":0.69224,"italic":0.0,"skew":0.0},"8739":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8741":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8743":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8744":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8745":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8746":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8747":{"depth":0.19444,"height":0.69444,"italic":0.11111,"skew":0.0},"8764":{"depth":-0.13313,"height":0.36687,"italic":0.0,"skew":0.0},"8768":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"8771":{"depth":-0.03625,"height":0.46375,"italic":0.0,"skew":0.0},"8776":{"depth":-0.01688,"height":0.48312,"italic":0.0,"skew":0.0},"8781":{"depth":-0.03625,"height":0.46375,"italic":0.0,"skew":0.0},"88":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"8801":{"depth":-0.03625,"height":0.46375,"italic":0.0,"skew":0.0},"8804":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"8805":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"8810":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"8811":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"8826":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"8827":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"8834":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"8835":{"depth":0.0391,"height":0.5391,"italic":0.0,"skew":0.0},"8838":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"8839":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"8846":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8849":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"8850":{"depth":0.13597,"height":0.63597,"italic":0.0,"skew":0.0},"8851":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8852":{"depth":0.0,"height":0.55556,"italic":0.0,"skew":0.0},"8853":{"depth":0.08333,"height":0.58333,"italic":0.0,"skew":0.0},"8854":{"depth":0.08333,"height":0.58333,"italic":0.0,"skew":0.0},"8855":{"depth":0.08333,"height":0.58333,"italic":0.0,"skew":0.0},"8856":{"depth":0.08333,"height":0.58333,"italic":0.0,"skew":0.0},"8857":{"depth":0.08333,"height":0.58333,"italic":0.0,"skew":0.0},"8866":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8867":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8868":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"8869":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"89":{"depth":0.0,"height":0.68333,"italic":0.025,"skew":0.0},"8900":{"depth":-0.05555,"height":0.44445,"italic":0.0,"skew":0.0},"8901":{"depth":-0.05555,"height":0.44445,"italic":0.0,"skew":0.0},"8902":{"depth":-0.03472,"height":0.46528,"italic":0.0,"skew":0.0},"8968":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8969":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8970":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8971":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"8994":{"depth":-0.14236,"height":0.35764,"italic":0.0,"skew":0.0},"8995":{"depth":-0.14236,"height":0.35764,"italic":0.0,"skew":0.0},"90":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"91":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"915":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"916":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"92":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"920":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"923":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"926":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"928":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"93":{"depth":0.25,"height":0.75,"italic":0.0,"skew":0.0},"931":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"933":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"934":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"936":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"937":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.0},"94":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"95":{"depth":0.31,"height":0.12056,"italic":0.02778,"skew":0.0},"96":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"9651":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"9657":{"depth":-0.03472,"height":0.46528,"italic":0.0,"skew":0.0},"9661":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"9667":{"depth":-0.03472,"height":0.46528,"italic":0.0,"skew":0.0},"97":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"9711":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"98":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"9824":{"depth":0.12963,"height":0.69444,"italic":0.0,"skew":0.0},"9825":{"depth":0.12963,"height":0.69444,"italic":0.0,"skew":0.0},"9826":{"depth":0.12963,"height":0.69444,"italic":0.0,"skew":0.0},"9827":{"depth":0.12963,"height":0.69444,"italic":0.0,"skew":0.0},"9837":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"9838":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"9839":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"99":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0}},"Math-BoldItalic":{"100":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"1009":{"depth":0.19444,"height":0.44444,"italic":0.0,"skew":0.0},"101":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"1013":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"102":{"depth":0.19444,"height":0.69444,"italic":0.11042,"skew":0.0},"103":{"depth":0.19444,"height":0.44444,"italic":0.03704,"skew":0.0},"104":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"105":{"depth":0.0,"height":0.69326,"italic":0.0,"skew":0.0},"106":{"depth":0.19444,"height":0.69326,"italic":0.0622,"skew":0.0},"107":{"depth":0.0,"height":0.69444,"italic":0.01852,"skew":0.0},"108":{"depth":0.0,"height":0.69444,"italic":0.0088,"skew":0.0},"109":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"110":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"111":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"112":{"depth":0.19444,"height":0.44444,"italic":0.0,"skew":0.0},"113":{"depth":0.19444,"height":0.44444,"italic":0.03704,"skew":0.0},"114":{"depth":0.0,"height":0.44444,"italic":0.03194,"skew":0.0},"115":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"116":{"depth":0.0,"height":0.63492,"italic":0.0,"skew":0.0},"117":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"118":{"depth":0.0,"height":0.44444,"italic":0.03704,"skew":0.0},"119":{"depth":0.0,"height":0.44444,"italic":0.02778,"skew":0.0},"120":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"121":{"depth":0.19444,"height":0.44444,"italic":0.03704,"skew":0.0},"122":{"depth":0.0,"height":0.44444,"italic":0.04213,"skew":0.0},"47":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"65":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"66":{"depth":0.0,"height":0.68611,"italic":0.04835,"skew":0.0},"67":{"depth":0.0,"height":0.68611,"italic":0.06979,"skew":0.0},"68":{"depth":0.0,"height":0.68611,"italic":0.03194,"skew":0.0},"69":{"depth":0.0,"height":0.68611,"italic":0.05451,"skew":0.0},"70":{"depth":0.0,"height":0.68611,"italic":0.15972,"skew":0.0},"71":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"72":{"depth":0.0,"height":0.68611,"italic":0.08229,"skew":0.0},"73":{"depth":0.0,"height":0.68611,"italic":0.07778,"skew":0.0},"74":{"depth":0.0,"height":0.68611,"italic":0.10069,"skew":0.0},"75":{"depth":0.0,"height":0.68611,"italic":0.06979,"skew":0.0},"76":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"77":{"depth":0.0,"height":0.68611,"italic":0.11424,"skew":0.0},"78":{"depth":0.0,"height":0.68611,"italic":0.11424,"skew":0.0},"79":{"depth":0.0,"height":0.68611,"italic":0.03194,"skew":0.0},"80":{"depth":0.0,"height":0.68611,"italic":0.15972,"skew":0.0},"81":{"depth":0.19444,"height":0.68611,"italic":0.0,"skew":0.0},"82":{"depth":0.0,"height":0.68611,"italic":0.00421,"skew":0.0},"83":{"depth":0.0,"height":0.68611,"italic":0.05382,"skew":0.0},"84":{"depth":0.0,"height":0.68611,"italic":0.15972,"skew":0.0},"85":{"depth":0.0,"height":0.68611,"italic":0.11424,"skew":0.0},"86":{"depth":0.0,"height":0.68611,"italic":0.25555,"skew":0.0},"87":{"depth":0.0,"height":0.68611,"italic":0.15972,"skew":0.0},"88":{"depth":0.0,"height":0.68611,"italic":0.07778,"skew":0.0},"89":{"depth":0.0,"height":0.68611,"italic":0.25555,"skew":0.0},"90":{"depth":0.0,"height":0.68611,"italic":0.06979,"skew":0.0},"915":{"depth":0.0,"height":0.68611,"italic":0.15972,"skew":0.0},"916":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"920":{"depth":0.0,"height":0.68611,"italic":0.03194,"skew":0.0},"923":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"926":{"depth":0.0,"height":0.68611,"italic":0.07458,"skew":0.0},"928":{"depth":0.0,"height":0.68611,"italic":0.08229,"skew":0.0},"931":{"depth":0.0,"height":0.68611,"italic":0.05451,"skew":0.0},"933":{"depth":0.0,"height":0.68611,"italic":0.15972,"skew":0.0},"934":{"depth":0.0,"height":0.68611,"italic":0.0,"skew":0.0},"936":{"depth":0.0,"height":0.68611,"italic":0.11653,"skew":0.0},"937":{"depth":0.0,"height":0.68611,"italic":0.04835,"skew":0.0},"945":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"946":{"depth":0.19444,"height":0.69444,"italic":0.03403,"skew":0.0},"947":{"depth":0.19444,"height":0.44444,"italic":0.06389,"skew":0.0},"948":{"depth":0.0,"height":0.69444,"italic":0.03819,"skew":0.0},"949":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"950":{"depth":0.19444,"height":0.69444,"italic":0.06215,"skew":0.0},"951":{"depth":0.19444,"height":0.44444,"italic":0.03704,"skew":0.0},"952":{"depth":0.0,"height":0.69444,"italic":0.03194,"skew":0.0},"953":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"954":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"955":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"956":{"depth":0.19444,"height":0.44444,"italic":0.0,"skew":0.0},"957":{"depth":0.0,"height":0.44444,"italic":0.06898,"skew":0.0},"958":{"depth":0.19444,"height":0.69444,"italic":0.03021,"skew":0.0},"959":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"960":{"depth":0.0,"height":0.44444,"italic":0.03704,"skew":0.0},"961":{"depth":0.19444,"height":0.44444,"italic":0.0,"skew":0.0},"962":{"depth":0.09722,"height":0.44444,"italic":0.07917,"skew":0.0},"963":{"depth":0.0,"height":0.44444,"italic":0.03704,"skew":0.0},"964":{"depth":0.0,"height":0.44444,"italic":0.13472,"skew":0.0},"965":{"depth":0.0,"height":0.44444,"italic":0.03704,"skew":0.0},"966":{"depth":0.19444,"height":0.44444,"italic":0.0,"skew":0.0},"967":{"depth":0.19444,"height":0.44444,"italic":0.0,"skew":0.0},"968":{"depth":0.19444,"height":0.69444,"italic":0.03704,"skew":0.0},"969":{"depth":0.0,"height":0.44444,"italic":0.03704,"skew":0.0},"97":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0},"977":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"98":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"981":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"982":{"depth":0.0,"height":0.44444,"italic":0.03194,"skew":0.0},"99":{"depth":0.0,"height":0.44444,"italic":0.0,"skew":0.0}},"Math-Italic":{"100":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.16667},"1009":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.08334},"101":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"1013":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"102":{"depth":0.19444,"height":0.69444,"italic":0.10764,"skew":0.16667},"103":{"depth":0.19444,"height":0.43056,"italic":0.03588,"skew":0.02778},"104":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"105":{"depth":0.0,"height":0.65952,"italic":0.0,"skew":0.0},"106":{"depth":0.19444,"height":0.65952,"italic":0.05724,"skew":0.0},"107":{"depth":0.0,"height":0.69444,"italic":0.03148,"skew":0.0},"108":{"depth":0.0,"height":0.69444,"italic":0.01968,"skew":0.08334},"109":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"110":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"111":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"112":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.08334},"113":{"depth":0.19444,"height":0.43056,"italic":0.03588,"skew":0.08334},"114":{"depth":0.0,"height":0.43056,"italic":0.02778,"skew":0.05556},"115":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"116":{"depth":0.0,"height":0.61508,"italic":0.0,"skew":0.08334},"117":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.02778},"118":{"depth":0.0,"height":0.43056,"italic":0.03588,"skew":0.02778},"119":{"depth":0.0,"height":0.43056,"italic":0.02691,"skew":0.08334},"120":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.02778},"121":{"depth":0.19444,"height":0.43056,"italic":0.03588,"skew":0.05556},"122":{"depth":0.0,"height":0.43056,"italic":0.04398,"skew":0.05556},"47":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.0},"65":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.13889},"66":{"depth":0.0,"height":0.68333,"italic":0.05017,"skew":0.08334},"67":{"depth":0.0,"height":0.68333,"italic":0.07153,"skew":0.08334},"68":{"depth":0.0,"height":0.68333,"italic":0.02778,"skew":0.05556},"69":{"depth":0.0,"height":0.68333,"italic":0.05764,"skew":0.08334},"70":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.08334},"71":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.08334},"72":{"depth":0.0,"height":0.68333,"italic":0.08125,"skew":0.05556},"73":{"depth":0.0,"height":0.68333,"italic":0.07847,"skew":0.11111},"74":{"depth":0.0,"height":0.68333,"italic":0.09618,"skew":0.16667},"75":{"depth":0.0,"height":0.68333,"italic":0.07153,"skew":0.05556},"76":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.02778},"77":{"depth":0.0,"height":0.68333,"italic":0.10903,"skew":0.08334},"78":{"depth":0.0,"height":0.68333,"italic":0.10903,"skew":0.08334},"79":{"depth":0.0,"height":0.68333,"italic":0.02778,"skew":0.08334},"80":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.08334},"81":{"depth":0.19444,"height":0.68333,"italic":0.0,"skew":0.08334},"82":{"depth":0.0,"height":0.68333,"italic":0.00773,"skew":0.08334},"83":{"depth":0.0,"height":0.68333,"italic":0.05764,"skew":0.08334},"84":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.08334},"85":{"depth":0.0,"height":0.68333,"italic":0.10903,"skew":0.02778},"86":{"depth":0.0,"height":0.68333,"italic":0.22222,"skew":0.0},"87":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.0},"88":{"depth":0.0,"height":0.68333,"italic":0.07847,"skew":0.08334},"89":{"depth":0.0,"height":0.68333,"italic":0.22222,"skew":0.0},"90":{"depth":0.0,"height":0.68333,"italic":0.07153,"skew":0.08334},"915":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.08334},"916":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.16667},"920":{"depth":0.0,"height":0.68333,"italic":0.02778,"skew":0.08334},"923":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.16667},"926":{"depth":0.0,"height":0.68333,"italic":0.07569,"skew":0.08334},"928":{"depth":0.0,"height":0.68333,"italic":0.08125,"skew":0.05556},"931":{"depth":0.0,"height":0.68333,"italic":0.05764,"skew":0.08334},"933":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.05556},"934":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.08334},"936":{"depth":0.0,"height":0.68333,"italic":0.11,"skew":0.05556},"937":{"depth":0.0,"height":0.68333,"italic":0.05017,"skew":0.08334},"945":{"depth":0.0,"height":0.43056,"italic":0.0037,"skew":0.02778},"946":{"depth":0.19444,"height":0.69444,"italic":0.05278,"skew":0.08334},"947":{"depth":0.19444,"height":0.43056,"italic":0.05556,"skew":0.0},"948":{"depth":0.0,"height":0.69444,"italic":0.03785,"skew":0.05556},"949":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.08334},"950":{"depth":0.19444,"height":0.69444,"italic":0.07378,"skew":0.08334},"951":{"depth":0.19444,"height":0.43056,"italic":0.03588,"skew":0.05556},"952":{"depth":0.0,"height":0.69444,"italic":0.02778,"skew":0.08334},"953":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"954":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"955":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"956":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.02778},"957":{"depth":0.0,"height":0.43056,"italic":0.06366,"skew":0.02778},"958":{"depth":0.19444,"height":0.69444,"italic":0.04601,"skew":0.11111},"959":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"960":{"depth":0.0,"height":0.43056,"italic":0.03588,"skew":0.0},"961":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.08334},"962":{"depth":0.09722,"height":0.43056,"italic":0.07986,"skew":0.08334},"963":{"depth":0.0,"height":0.43056,"italic":0.03588,"skew":0.0},"964":{"depth":0.0,"height":0.43056,"italic":0.1132,"skew":0.02778},"965":{"depth":0.0,"height":0.43056,"italic":0.03588,"skew":0.02778},"966":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.08334},"967":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.05556},"968":{"depth":0.19444,"height":0.69444,"italic":0.03588,"skew":0.11111},"969":{"depth":0.0,"height":0.43056,"italic":0.03588,"skew":0.0},"97":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"977":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.08334},"98":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"981":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.08334},"982":{"depth":0.0,"height":0.43056,"italic":0.02778,"skew":0.0},"99":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556}},"Math-Regular":{"100":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.16667},"1009":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.08334},"101":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"1013":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"102":{"depth":0.19444,"height":0.69444,"italic":0.10764,"skew":0.16667},"103":{"depth":0.19444,"height":0.43056,"italic":0.03588,"skew":0.02778},"104":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"105":{"depth":0.0,"height":0.65952,"italic":0.0,"skew":0.0},"106":{"depth":0.19444,"height":0.65952,"italic":0.05724,"skew":0.0},"107":{"depth":0.0,"height":0.69444,"italic":0.03148,"skew":0.0},"108":{"depth":0.0,"height":0.69444,"italic":0.01968,"skew":0.08334},"109":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"110":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"111":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"112":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.08334},"113":{"depth":0.19444,"height":0.43056,"italic":0.03588,"skew":0.08334},"114":{"depth":0.0,"height":0.43056,"italic":0.02778,"skew":0.05556},"115":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"116":{"depth":0.0,"height":0.61508,"italic":0.0,"skew":0.08334},"117":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.02778},"118":{"depth":0.0,"height":0.43056,"italic":0.03588,"skew":0.02778},"119":{"depth":0.0,"height":0.43056,"italic":0.02691,"skew":0.08334},"120":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.02778},"121":{"depth":0.19444,"height":0.43056,"italic":0.03588,"skew":0.05556},"122":{"depth":0.0,"height":0.43056,"italic":0.04398,"skew":0.05556},"65":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.13889},"66":{"depth":0.0,"height":0.68333,"italic":0.05017,"skew":0.08334},"67":{"depth":0.0,"height":0.68333,"italic":0.07153,"skew":0.08334},"68":{"depth":0.0,"height":0.68333,"italic":0.02778,"skew":0.05556},"69":{"depth":0.0,"height":0.68333,"italic":0.05764,"skew":0.08334},"70":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.08334},"71":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.08334},"72":{"depth":0.0,"height":0.68333,"italic":0.08125,"skew":0.05556},"73":{"depth":0.0,"height":0.68333,"italic":0.07847,"skew":0.11111},"74":{"depth":0.0,"height":0.68333,"italic":0.09618,"skew":0.16667},"75":{"depth":0.0,"height":0.68333,"italic":0.07153,"skew":0.05556},"76":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.02778},"77":{"depth":0.0,"height":0.68333,"italic":0.10903,"skew":0.08334},"78":{"depth":0.0,"height":0.68333,"italic":0.10903,"skew":0.08334},"79":{"depth":0.0,"height":0.68333,"italic":0.02778,"skew":0.08334},"80":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.08334},"81":{"depth":0.19444,"height":0.68333,"italic":0.0,"skew":0.08334},"82":{"depth":0.0,"height":0.68333,"italic":0.00773,"skew":0.08334},"83":{"depth":0.0,"height":0.68333,"italic":0.05764,"skew":0.08334},"84":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.08334},"85":{"depth":0.0,"height":0.68333,"italic":0.10903,"skew":0.02778},"86":{"depth":0.0,"height":0.68333,"italic":0.22222,"skew":0.0},"87":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.0},"88":{"depth":0.0,"height":0.68333,"italic":0.07847,"skew":0.08334},"89":{"depth":0.0,"height":0.68333,"italic":0.22222,"skew":0.0},"90":{"depth":0.0,"height":0.68333,"italic":0.07153,"skew":0.08334},"915":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.08334},"916":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.16667},"920":{"depth":0.0,"height":0.68333,"italic":0.02778,"skew":0.08334},"923":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.16667},"926":{"depth":0.0,"height":0.68333,"italic":0.07569,"skew":0.08334},"928":{"depth":0.0,"height":0.68333,"italic":0.08125,"skew":0.05556},"931":{"depth":0.0,"height":0.68333,"italic":0.05764,"skew":0.08334},"933":{"depth":0.0,"height":0.68333,"italic":0.13889,"skew":0.05556},"934":{"depth":0.0,"height":0.68333,"italic":0.0,"skew":0.08334},"936":{"depth":0.0,"height":0.68333,"italic":0.11,"skew":0.05556},"937":{"depth":0.0,"height":0.68333,"italic":0.05017,"skew":0.08334},"945":{"depth":0.0,"height":0.43056,"italic":0.0037,"skew":0.02778},"946":{"depth":0.19444,"height":0.69444,"italic":0.05278,"skew":0.08334},"947":{"depth":0.19444,"height":0.43056,"italic":0.05556,"skew":0.0},"948":{"depth":0.0,"height":0.69444,"italic":0.03785,"skew":0.05556},"949":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.08334},"950":{"depth":0.19444,"height":0.69444,"italic":0.07378,"skew":0.08334},"951":{"depth":0.19444,"height":0.43056,"italic":0.03588,"skew":0.05556},"952":{"depth":0.0,"height":0.69444,"italic":0.02778,"skew":0.08334},"953":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"954":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"955":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"956":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.02778},"957":{"depth":0.0,"height":0.43056,"italic":0.06366,"skew":0.02778},"958":{"depth":0.19444,"height":0.69444,"italic":0.04601,"skew":0.11111},"959":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556},"960":{"depth":0.0,"height":0.43056,"italic":0.03588,"skew":0.0},"961":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.08334},"962":{"depth":0.09722,"height":0.43056,"italic":0.07986,"skew":0.08334},"963":{"depth":0.0,"height":0.43056,"italic":0.03588,"skew":0.0},"964":{"depth":0.0,"height":0.43056,"italic":0.1132,"skew":0.02778},"965":{"depth":0.0,"height":0.43056,"italic":0.03588,"skew":0.02778},"966":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.08334},"967":{"depth":0.19444,"height":0.43056,"italic":0.0,"skew":0.05556},"968":{"depth":0.19444,"height":0.69444,"italic":0.03588,"skew":0.11111},"969":{"depth":0.0,"height":0.43056,"italic":0.03588,"skew":0.0},"97":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.0},"977":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.08334},"98":{"depth":0.0,"height":0.69444,"italic":0.0,"skew":0.0},"981":{"depth":0.19444,"height":0.69444,"italic":0.0,"skew":0.08334},"982":{"depth":0.0,"height":0.43056,"italic":0.02778,"skew":0.0},"99":{"depth":0.0,"height":0.43056,"italic":0.0,"skew":0.05556}},"Size1-Regular":{"8748":{"depth":0.306,"height":0.805,"italic":0.19445,"skew":0.0},"8749":{"depth":0.306,"height":0.805,"italic":0.19445,"skew":0.0},"10216":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"10217":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"10752":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"10753":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"10754":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"10756":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"10758":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"123":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"125":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"40":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"41":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"47":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"710":{"depth":0.0,"height":0.72222,"italic":0.0,"skew":0.0},"732":{"depth":0.0,"height":0.72222,"italic":0.0,"skew":0.0},"770":{"depth":0.0,"height":0.72222,"italic":0.0,"skew":0.0},"771":{"depth":0.0,"height":0.72222,"italic":0.0,"skew":0.0},"8214":{"depth":-0.00099,"height":0.601,"italic":0.0,"skew":0.0},"8593":{"depth":1e-05,"height":0.6,"italic":0.0,"skew":0.0},"8595":{"depth":1e-05,"height":0.6,"italic":0.0,"skew":0.0},"8657":{"depth":1e-05,"height":0.6,"italic":0.0,"skew":0.0},"8659":{"depth":1e-05,"height":0.6,"italic":0.0,"skew":0.0},"8719":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"8720":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"8721":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"8730":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"8739":{"depth":-0.00599,"height":0.606,"italic":0.0,"skew":0.0},"8741":{"depth":-0.00599,"height":0.606,"italic":0.0,"skew":0.0},"8747":{"depth":0.30612,"height":0.805,"italic":0.19445,"skew":0.0},"8750":{"depth":0.30612,"height":0.805,"italic":0.19445,"skew":0.0},"8896":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"8897":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"8898":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"8899":{"depth":0.25001,"height":0.75,"italic":0.0,"skew":0.0},"8968":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"8969":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"8970":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"8971":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"91":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"9168":{"depth":-0.00099,"height":0.601,"italic":0.0,"skew":0.0},"92":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0},"93":{"depth":0.35001,"height":0.85,"italic":0.0,"skew":0.0}},"Size2-Regular":{"8748":{"depth":0.862,"height":1.36,"italic":0.44445,"skew":0.0},"8749":{"depth":0.862,"height":1.36,"italic":0.44445,"skew":0.0},"10216":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"10217":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"10752":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"10753":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"10754":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"10756":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"10758":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"123":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"125":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"40":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"41":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"47":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"710":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"732":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"770":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"771":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"8719":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"8720":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"8721":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"8730":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"8747":{"depth":0.86225,"height":1.36,"italic":0.44445,"skew":0.0},"8750":{"depth":0.86225,"height":1.36,"italic":0.44445,"skew":0.0},"8896":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"8897":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"8898":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"8899":{"depth":0.55001,"height":1.05,"italic":0.0,"skew":0.0},"8968":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"8969":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"8970":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"8971":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"91":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"92":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"93":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0}},"Size3-Regular":{"10216":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"10217":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"123":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"125":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"40":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"41":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"47":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"710":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"732":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"770":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"771":{"depth":0.0,"height":0.75,"italic":0.0,"skew":0.0},"8730":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"8968":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"8969":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"8970":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"8971":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"91":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"92":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0},"93":{"depth":0.95003,"height":1.45,"italic":0.0,"skew":0.0}},"Size4-Regular":{"10216":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"10217":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"123":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"125":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"40":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"41":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"47":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"57344":{"depth":-0.00499,"height":0.605,"italic":0.0,"skew":0.0},"57345":{"depth":-0.00499,"height":0.605,"italic":0.0,"skew":0.0},"57680":{"depth":0.0,"height":0.12,"italic":0.0,"skew":0.0},"57681":{"depth":0.0,"height":0.12,"italic":0.0,"skew":0.0},"57682":{"depth":0.0,"height":0.12,"italic":0.0,"skew":0.0},"57683":{"depth":0.0,"height":0.12,"italic":0.0,"skew":0.0},"710":{"depth":0.0,"height":0.825,"italic":0.0,"skew":0.0},"732":{"depth":0.0,"height":0.825,"italic":0.0,"skew":0.0},"770":{"depth":0.0,"height":0.825,"italic":0.0,"skew":0.0},"771":{"depth":0.0,"height":0.825,"italic":0.0,"skew":0.0},"8730":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"8968":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"8969":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"8970":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"8971":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"91":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"9115":{"depth":0.64502,"height":1.155,"italic":0.0,"skew":0.0},"9116":{"depth":1e-05,"height":0.6,"italic":0.0,"skew":0.0},"9117":{"depth":0.64502,"height":1.155,"italic":0.0,"skew":0.0},"9118":{"depth":0.64502,"height":1.155,"italic":0.0,"skew":0.0},"9119":{"depth":1e-05,"height":0.6,"italic":0.0,"skew":0.0},"9120":{"depth":0.64502,"height":1.155,"italic":0.0,"skew":0.0},"9121":{"depth":0.64502,"height":1.155,"italic":0.0,"skew":0.0},"9122":{"depth":-0.00099,"height":0.601,"italic":0.0,"skew":0.0},"9123":{"depth":0.64502,"height":1.155,"italic":0.0,"skew":0.0},"9124":{"depth":0.64502,"height":1.155,"italic":0.0,"skew":0.0},"9125":{"depth":-0.00099,"height":0.601,"italic":0.0,"skew":0.0},"9126":{"depth":0.64502,"height":1.155,"italic":0.0,"skew":0.0},"9127":{"depth":1e-05,"height":0.9,"italic":0.0,"skew":0.0},"9128":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"9129":{"depth":0.90001,"height":0.0,"italic":0.0,"skew":0.0},"9130":{"depth":0.0,"height":0.3,"italic":0.0,"skew":0.0},"9131":{"depth":1e-05,"height":0.9,"italic":0.0,"skew":0.0},"9132":{"depth":0.65002,"height":1.15,"italic":0.0,"skew":0.0},"9133":{"depth":0.90001,"height":0.0,"italic":0.0,"skew":0.0},"9143":{"depth":0.88502,"height":0.915,"italic":0.0,"skew":0.0},"92":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0},"93":{"depth":1.25003,"height":1.75,"italic":0.0,"skew":0.0}}};

/**
 * This function is a convience function for looking up information in the
 * metricMap table. It takes a character as a string, and a style
 */
var getCharacterMetrics = function(character, style) {
    return metricMap[style][character.charCodeAt(0)];
};

module.exports = {
    metrics: metrics,
    getCharacterMetrics: getCharacterMetrics
};

},{"./Style":7}],13:[function(require,module,exports){
var utils = require("./utils");
var ParseError = require("./ParseError");

// This file contains a list of functions that we parse. The functions map
// contains the following data:

/*
 * Keys are the name of the functions to parse
 * The data contains the following keys:
 *  - numArgs: The number of arguments the function takes.
 *  - argTypes: (optional) An array corresponding to each argument of the
 *              function, giving the type of argument that should be parsed. Its
 *              length should be equal to `numArgs + numOptionalArgs`. Valid
 *              types:
 *               - "size": A size-like thing, such as "1em" or "5ex"
 *               - "color": An html color, like "#abc" or "blue"
 *               - "original": The same type as the environment that the
 *                             function being parsed is in (e.g. used for the
 *                             bodies of functions like \color where the first
 *                             argument is special and the second argument is
 *                             parsed normally)
 *              Other possible types (probably shouldn't be used)
 *               - "text": Text-like (e.g. \text)
 *               - "math": Normal math
 *              If undefined, this will be treated as an appropriate length
 *              array of "original" strings
 *  - greediness: (optional) The greediness of the function to use ungrouped
 *                arguments.
 *
 *                E.g. if you have an expression
 *                  \sqrt \frac 1 2
 *                since \frac has greediness=2 vs \sqrt's greediness=1, \frac
 *                will use the two arguments '1' and '2' as its two arguments,
 *                then that whole function will be used as the argument to
 *                \sqrt. On the other hand, the expressions
 *                  \frac \frac 1 2 3
 *                and
 *                  \frac \sqrt 1 2
 *                will fail because \frac and \frac have equal greediness
 *                and \sqrt has a lower greediness than \frac respectively. To
 *                make these parse, we would have to change them to:
 *                  \frac {\frac 1 2} 3
 *                and
 *                  \frac {\sqrt 1} 2
 *
 *                The default value is `1`
 *  - allowedInText: (optional) Whether or not the function is allowed inside
 *                   text mode (default false)
 *  - numOptionalArgs: (optional) The number of optional arguments the function
 *                     should parse. If the optional arguments aren't found,
 *                     `null` will be passed to the handler in their place.
 *                     (default 0)
 *  - handler: The function that is called to handle this function and its
 *             arguments. The arguments are:
 *              - func: the text of the function
 *              - [args]: the next arguments are the arguments to the function,
 *                        of which there are numArgs of them
 *              - positions: the positions in the overall string of the function
 *                           and the arguments. Should only be used to produce
 *                           error messages
 *             The function should return an object with the following keys:
 *              - type: The type of element that this is. This is then used in
 *                      buildTree to determine which function should be called
 *                      to build this node into a DOM node
 *             Any other data can be added to the object, which will be passed
 *             in to the function in buildTree as `group.value`.
 */

var functions = {
    // A normal square root
    "\\sqrt": {
        numArgs: 1,
        numOptionalArgs: 1,
        handler: function(func, optional, body, positions) {
            if (optional != null) {
                throw new ParseError(
                    "Optional arguments to \\sqrt aren't supported yet",
                    this.lexer, positions[1] - 1);
            }

            return {
                type: "sqrt",
                body: body
            };
        }
    },

    // Some non-mathy text
    "\\text": {
        numArgs: 1,
        argTypes: ["text"],
        greediness: 2,
        handler: function(func, body) {
            // Since the corresponding buildTree function expects a list of
            // elements, we normalize for different kinds of arguments
            // TODO(emily): maybe this should be done somewhere else
            var inner;
            if (body.type === "ordgroup") {
                inner = body.value;
            } else {
                inner = [body];
            }

            return {
                type: "text",
                body: inner
            };
        }
    },

    // A two-argument custom color
    "\\color": {
        numArgs: 2,
        allowedInText: true,
        argTypes: ["color", "original"],
        handler: function(func, color, body) {
            // Normalize the different kinds of bodies (see \text above)
            var inner;
            if (body.type === "ordgroup") {
                inner = body.value;
            } else {
                inner = [body];
            }

            return {
                type: "color",
                color: color.value,
                value: inner
            };
        }
    },

    // An overline
    "\\overline": {
        numArgs: 1,
        handler: function(func, body) {
            return {
                type: "overline",
                body: body
            };
        }
    },

    // A box of the width and height
    "\\rule": {
        numArgs: 2,
        numOptionalArgs: 1,
        argTypes: ["size", "size", "size"],
        handler: function(func, shift, width, height) {
            return {
                type: "rule",
                shift: shift && shift.value,
                width: width.value,
                height: height.value
            };
        }
    },

    // A KaTeX logo
    "\\KaTeX": {
        numArgs: 0,
        handler: function(func) {
            return {
                type: "katex"
            };
        }
    }
};

// Extra data needed for the delimiter handler down below
var delimiterSizes = {
    "\\bigl" : {type: "open",    size: 1},
    "\\Bigl" : {type: "open",    size: 2},
    "\\biggl": {type: "open",    size: 3},
    "\\Biggl": {type: "open",    size: 4},
    "\\bigr" : {type: "close",   size: 1},
    "\\Bigr" : {type: "close",   size: 2},
    "\\biggr": {type: "close",   size: 3},
    "\\Biggr": {type: "close",   size: 4},
    "\\bigm" : {type: "rel",     size: 1},
    "\\Bigm" : {type: "rel",     size: 2},
    "\\biggm": {type: "rel",     size: 3},
    "\\Biggm": {type: "rel",     size: 4},
    "\\big"  : {type: "textord", size: 1},
    "\\Big"  : {type: "textord", size: 2},
    "\\bigg" : {type: "textord", size: 3},
    "\\Bigg" : {type: "textord", size: 4}
};

var delimiters = [
    "(", ")", "[", "\\lbrack", "]", "\\rbrack",
    "\\{", "\\lbrace", "\\}", "\\rbrace",
    "\\lfloor", "\\rfloor", "\\lceil", "\\rceil",
    "<", ">", "\\langle", "\\rangle",
    "/", "\\backslash",
    "|", "\\vert", "\\|", "\\Vert",
    "\\uparrow", "\\Uparrow",
    "\\downarrow", "\\Downarrow",
    "\\updownarrow", "\\Updownarrow",
    "."
];

/*
 * This is a list of functions which each have the same function but have
 * different names so that we don't have to duplicate the data a bunch of times.
 * Each element in the list is an object with the following keys:
 *  - funcs: A list of function names to be associated with the data
 *  - data: An objecty with the same data as in each value of the `function`
 *          table above
 */
var duplicatedFunctions = [
    // Single-argument color functions
    {
        funcs: [
            "\\blue", "\\orange", "\\pink", "\\red",
            "\\green", "\\gray", "\\purple"
        ],
        data: {
            numArgs: 1,
            allowedInText: true,
            handler: function(func, body) {
                var atoms;
                if (body.type === "ordgroup") {
                    atoms = body.value;
                } else {
                    atoms = [body];
                }

                return {
                    type: "color",
                    color: "katex-" + func.slice(1),
                    value: atoms
                };
            }
        }
    },

    // There are 2 flags for operators; whether they produce limits in
    // displaystyle, and whether they are symbols and should grow in
    // displaystyle. These four groups cover the four possible choices.

    // No limits, not symbols
    {
        funcs: [
            "\\arcsin", "\\arccos", "\\arctan", "\\arg", "\\cos", "\\cosh",
            "\\cot", "\\coth", "\\csc", "\\deg", "\\dim", "\\exp", "\\hom",
            "\\ker", "\\lg", "\\ln", "\\log", "\\sec", "\\sin", "\\sinh",
            "\\tan","\\tanh"
        ],
        data: {
            numArgs: 0,
            handler: function(func) {
                return {
                    type: "op",
                    limits: false,
                    symbol: false,
                    body: func
                };
            }
        }
    },

    // Limits, not symbols
    {
        funcs: [
            "\\det", "\\gcd", "\\inf", "\\lim", "\\liminf", "\\limsup", "\\max",
            "\\min", "\\Pr", "\\sup"
        ],
        data: {
            numArgs: 0,
            handler: function(func) {
                return {
                    type: "op",
                    limits: true,
                    symbol: false,
                    body: func
                };
            }
        }
    },

    // No limits, symbols
    {
        funcs: [
            "\\int", "\\iint", "\\iiint", "\\oint"
        ],
        data: {
            numArgs: 0,
            handler: function(func) {
                return {
                    type: "op",
                    limits: false,
                    symbol: true,
                    body: func
                };
            }
        }
    },

    // Limits, symbols
    {
        funcs: [
            "\\coprod", "\\bigvee", "\\bigwedge", "\\biguplus", "\\bigcap",
            "\\bigcup", "\\intop", "\\prod", "\\sum", "\\bigotimes",
            "\\bigoplus", "\\bigodot", "\\bigsqcup", "\\smallint"
        ],
        data: {
            numArgs: 0,
            handler: function(func) {
                return {
                    type: "op",
                    limits: true,
                    symbol: true,
                    body: func
                };
            }
        }
    },

    // Fractions
    {
        funcs: [
            "\\dfrac", "\\frac", "\\tfrac",
            "\\dbinom", "\\binom", "\\tbinom"
        ],
        data: {
            numArgs: 2,
            greediness: 2,
            handler: function(func, numer, denom) {
                var hasBarLine;
                var leftDelim = null;
                var rightDelim = null;
                var size = "auto";

                switch (func) {
                    case "\\dfrac":
                    case "\\frac":
                    case "\\tfrac":
                        hasBarLine = true;
                        break;
                    case "\\dbinom":
                    case "\\binom":
                    case "\\tbinom":
                        hasBarLine = false;
                        leftDelim = "(";
                        rightDelim = ")";
                        break;
                    default:
                        throw new Error("Unrecognized genfrac command");
                }

                switch (func) {
                    case "\\dfrac":
                    case "\\dbinom":
                        size = "display";
                        break;
                    case "\\tfrac":
                    case "\\tbinom":
                        size = "text";
                        break;
                }

                return {
                    type: "genfrac",
                    numer: numer,
                    denom: denom,
                    hasBarLine: hasBarLine,
                    leftDelim: leftDelim,
                    rightDelim: rightDelim,
                    size: size
                };
            }
        }
    },

    // Left and right overlap functions
    {
        funcs: ["\\llap", "\\rlap"],
        data: {
            numArgs: 1,
            allowedInText: true,
            handler: function(func, body) {
                return {
                    type: func.slice(1),
                    body: body
                };
            }
        }
    },

    // Delimiter functions
    {
        funcs: [
            "\\bigl", "\\Bigl", "\\biggl", "\\Biggl",
            "\\bigr", "\\Bigr", "\\biggr", "\\Biggr",
            "\\bigm", "\\Bigm", "\\biggm", "\\Biggm",
            "\\big",  "\\Big",  "\\bigg",  "\\Bigg",
            "\\left", "\\right"
        ],
        data: {
            numArgs: 1,
            handler: function(func, delim, positions) {
                if (!utils.contains(delimiters, delim.value)) {
                    throw new ParseError(
                        "Invalid delimiter: '" + delim.value + "' after '" +
                            func + "'",
                        this.lexer, positions[1]);
                }

                // left and right are caught somewhere in Parser.js, which is
                // why this data doesn't match what is in buildTree
                if (func === "\\left" || func === "\\right") {
                    return {
                        type: "leftright",
                        value: delim.value
                    };
                } else {
                    return {
                        type: "delimsizing",
                        size: delimiterSizes[func].size,
                        delimType: delimiterSizes[func].type,
                        value: delim.value
                    };
                }
            }
        }
    },

    // Sizing functions (handled in Parser.js explicitly, hence no handler)
    {
        funcs: [
            "\\tiny", "\\scriptsize", "\\footnotesize", "\\small",
            "\\normalsize", "\\large", "\\Large", "\\LARGE", "\\huge", "\\Huge"
        ],
        data: {
            numArgs: 0
        }
    },

    // Style changing functions (handled in Parser.js explicitly, hence no
    // handler)
    {
        funcs: [
            "\\displaystyle", "\\textstyle", "\\scriptstyle",
            "\\scriptscriptstyle"
        ],
        data: {
            numArgs: 0
        }
    },

    // Accents
    {
        funcs: [
            "\\acute", "\\grave", "\\ddot", "\\tilde", "\\bar", "\\breve",
            "\\check", "\\hat", "\\vec", "\\dot"
            // We don't support expanding accents yet
            // "\\widetilde", "\\widehat"
        ],
        data: {
            numArgs: 1,
            handler: function(func, base) {
                return {
                    type: "accent",
                    accent: func,
                    base: base
                };
            }
        }
    },

    // Infix generalized fractions
    {
        funcs: ["\\over", "\\choose"],
        data: {
            numArgs: 0,
            handler: function (func) {
                var replaceWith;
                switch (func) {
                    case "\\over":
                        replaceWith = "\\frac";
                        break;
                    case "\\choose":
                        replaceWith = "\\binom";
                        break;
                    default:
                        throw new Error("Unrecognized infix genfrac command");
                }
                return {
                    type: "infix",
                    replaceWith: replaceWith
                };
            }
        }
    }
];

var addFuncsWithData = function(funcs, data) {
    for (var i = 0; i < funcs.length; i++) {
        functions[funcs[i]] = data;
    }
};

// Add all of the functions in duplicatedFunctions to the functions map
for (var i = 0; i < duplicatedFunctions.length; i++) {
    addFuncsWithData(duplicatedFunctions[i].funcs, duplicatedFunctions[i].data);
}

// Returns the greediness of a given function. Since greediness is optional, we
// use this function to put in the default value if it is undefined.
var getGreediness = function(func) {
    if (functions[func].greediness === undefined) {
        return 1;
    } else {
        return functions[func].greediness;
    }
};

// Set default values of functions
for (var f in functions) {
    if (functions.hasOwnProperty(f)) {
        var func = functions[f];

        functions[f] = {
            numArgs: func.numArgs,
            argTypes: func.argTypes,
            greediness: (func.greediness === undefined) ? 1 : func.greediness,
            allowedInText: func.allowedInText ? func.allowedInText : false,
            numOptionalArgs: (func.numOptionalArgs === undefined) ? 0 :
                func.numOptionalArgs,
            handler: func.handler
        };
    }
}

module.exports = {
    funcs: functions,
    getGreediness: getGreediness
};

},{"./ParseError":5,"./utils":16}],14:[function(require,module,exports){
/**
 * Provides a single function for parsing an expression using a Parser
 * TODO(emily): Remove this
 */

var Parser = require("./Parser");

/**
 * Parses an expression using a Parser, then returns the parsed result.
 */
var parseTree = function(toParse) {
    var parser = new Parser(toParse);

    return parser.parse();
};

module.exports = parseTree;

},{"./Parser":6}],15:[function(require,module,exports){
/**
 * This file holds a list of all no-argument functions and single-character
 * symbols (like 'a' or ';').
 *
 * For each of the symbols, there are three properties they can have:
 * - font (required): the font to be used for this symbol. Either "main" (the
     normal font), or "ams" (the ams fonts).
 * - group (required): the ParseNode group type the symbol should have (i.e.
     "textord", "mathord", etc).
 * - replace (optional): the character that this symbol or function should be
 *   replaced with (i.e. "\phi" has a replace value of "\u03d5", the phi
 *   character in the main font).
 *
 * The outermost map in the table indicates what mode the symbols should be
 * accepted in (e.g. "math" or "text").
 */

var symbols = {
    "math": {
        "`": {
            font: "main",
            group: "textord",
            replace: "\u2018"
        },
        "\\$": {
            font: "main",
            group: "textord",
            replace: "$"
        },
        "\\%": {
            font: "main",
            group: "textord",
            replace: "%"
        },
        "\\_": {
            font: "main",
            group: "textord",
            replace: "_"
        },
        "\\angle": {
            font: "main",
            group: "textord",
            replace: "\u2220"
        },
        "\\infty": {
            font: "main",
            group: "textord",
            replace: "\u221e"
        },
        "\\prime": {
            font: "main",
            group: "textord",
            replace: "\u2032"
        },
        "\\triangle": {
            font: "main",
            group: "textord",
            replace: "\u25b3"
        },
        "\\Gamma": {
            font: "main",
            group: "textord",
            replace: "\u0393"
        },
        "\\Delta": {
            font: "main",
            group: "textord",
            replace: "\u0394"
        },
        "\\Theta": {
            font: "main",
            group: "textord",
            replace: "\u0398"
        },
        "\\Lambda": {
            font: "main",
            group: "textord",
            replace: "\u039b"
        },
        "\\Xi": {
            font: "main",
            group: "textord",
            replace: "\u039e"
        },
        "\\Pi": {
            font: "main",
            group: "textord",
            replace: "\u03a0"
        },
        "\\Sigma": {
            font: "main",
            group: "textord",
            replace: "\u03a3"
        },
        "\\Upsilon": {
            font: "main",
            group: "textord",
            replace: "\u03a5"
        },
        "\\Phi": {
            font: "main",
            group: "textord",
            replace: "\u03a6"
        },
        "\\Psi": {
            font: "main",
            group: "textord",
            replace: "\u03a8"
        },
        "\\Omega": {
            font: "main",
            group: "textord",
            replace: "\u03a9"
        },
        "\\neg": {
            font: "main",
            group: "textord",
            replace: "\u00ac"
        },
        "\\lnot": {
            font: "main",
            group: "textord",
            replace: "\u00ac"
        },
        "\\top": {
            font: "main",
            group: "textord",
            replace: "\u22a4"
        },
        "\\bot": {
            font: "main",
            group: "textord",
            replace: "\u22a5"
        },
        "\\emptyset": {
            font: "main",
            group: "textord",
            replace: "\u2205"
        },
        "\\varnothing": {
            font: "ams",
            group: "textord",
            replace: "\u2205"
        },
        "\\alpha": {
            font: "main",
            group: "mathord",
            replace: "\u03b1"
        },
        "\\beta": {
            font: "main",
            group: "mathord",
            replace: "\u03b2"
        },
        "\\gamma": {
            font: "main",
            group: "mathord",
            replace: "\u03b3"
        },
        "\\delta": {
            font: "main",
            group: "mathord",
            replace: "\u03b4"
        },
        "\\epsilon": {
            font: "main",
            group: "mathord",
            replace: "\u03f5"
        },
        "\\zeta": {
            font: "main",
            group: "mathord",
            replace: "\u03b6"
        },
        "\\eta": {
            font: "main",
            group: "mathord",
            replace: "\u03b7"
        },
        "\\theta": {
            font: "main",
            group: "mathord",
            replace: "\u03b8"
        },
        "\\iota": {
            font: "main",
            group: "mathord",
            replace: "\u03b9"
        },
        "\\kappa": {
            font: "main",
            group: "mathord",
            replace: "\u03ba"
        },
        "\\lambda": {
            font: "main",
            group: "mathord",
            replace: "\u03bb"
        },
        "\\mu": {
            font: "main",
            group: "mathord",
            replace: "\u03bc"
        },
        "\\nu": {
            font: "main",
            group: "mathord",
            replace: "\u03bd"
        },
        "\\xi": {
            font: "main",
            group: "mathord",
            replace: "\u03be"
        },
        "\\omicron": {
            font: "main",
            group: "mathord",
            replace: "o"
        },
        "\\pi": {
            font: "main",
            group: "mathord",
            replace: "\u03c0"
        },
        "\\rho": {
            font: "main",
            group: "mathord",
            replace: "\u03c1"
        },
        "\\sigma": {
            font: "main",
            group: "mathord",
            replace: "\u03c3"
        },
        "\\tau": {
            font: "main",
            group: "mathord",
            replace: "\u03c4"
        },
        "\\upsilon": {
            font: "main",
            group: "mathord",
            replace: "\u03c5"
        },
        "\\phi": {
            font: "main",
            group: "mathord",
            replace: "\u03d5"
        },
        "\\chi": {
            font: "main",
            group: "mathord",
            replace: "\u03c7"
        },
        "\\psi": {
            font: "main",
            group: "mathord",
            replace: "\u03c8"
        },
        "\\omega": {
            font: "main",
            group: "mathord",
            replace: "\u03c9"
        },
        "\\varepsilon": {
            font: "main",
            group: "mathord",
            replace: "\u03b5"
        },
        "\\vartheta": {
            font: "main",
            group: "mathord",
            replace: "\u03d1"
        },
        "\\varpi": {
            font: "main",
            group: "mathord",
            replace: "\u03d6"
        },
        "\\varrho": {
            font: "main",
            group: "mathord",
            replace: "\u03f1"
        },
        "\\varsigma": {
            font: "main",
            group: "mathord",
            replace: "\u03c2"
        },
        "\\varphi": {
            font: "main",
            group: "mathord",
            replace: "\u03c6"
        },
        "*": {
            font: "main",
            group: "bin",
            replace: "\u2217"
        },
        "+": {
            font: "main",
            group: "bin"
        },
        "-": {
            font: "main",
            group: "bin",
            replace: "\u2212"
        },
        "\\cdot": {
            font: "main",
            group: "bin",
            replace: "\u22c5"
        },
        "\\circ": {
            font: "main",
            group: "bin",
            replace: "\u2218"
        },
        "\\div": {
            font: "main",
            group: "bin",
            replace: "\u00f7"
        },
        "\\pm": {
            font: "main",
            group: "bin",
            replace: "\u00b1"
        },
        "\\times": {
            font: "main",
            group: "bin",
            replace: "\u00d7"
        },
        "\\cap": {
            font: "main",
            group: "bin",
            replace: "\u2229"
        },
        "\\cup": {
            font: "main",
            group: "bin",
            replace: "\u222a"
        },
        "\\setminus": {
            font: "main",
            group: "bin",
            replace: "\u2216"
        },
        "\\land": {
            font: "main",
            group: "bin",
            replace: "\u2227"
        },
        "\\lor": {
            font: "main",
            group: "bin",
            replace: "\u2228"
        },
        "\\wedge": {
            font: "main",
            group: "bin",
            replace: "\u2227"
        },
        "\\vee": {
            font: "main",
            group: "bin",
            replace: "\u2228"
        },
        "\\surd": {
            font: "main",
            group: "textord",
            replace: "\u221a"
        },
        "(": {
            font: "main",
            group: "open"
        },
        "[": {
            font: "main",
            group: "open"
        },
        "\\langle": {
            font: "main",
            group: "open",
            replace: "\u27e8"
        },
        "\\lvert": {
            font: "main",
            group: "open",
            replace: "\u2223"
        },
        ")": {
            font: "main",
            group: "close"
        },
        "]": {
            font: "main",
            group: "close"
        },
        "?": {
            font: "main",
            group: "close"
        },
        "!": {
            font: "main",
            group: "close"
        },
        "\\rangle": {
            font: "main",
            group: "close",
            replace: "\u27e9"
        },
        "\\rvert": {
            font: "main",
            group: "close",
            replace: "\u2223"
        },
        "=": {
            font: "main",
            group: "rel"
        },
        "<": {
            font: "main",
            group: "rel"
        },
        ">": {
            font: "main",
            group: "rel"
        },
        ":": {
            font: "main",
            group: "rel"
        },
        "\\approx": {
            font: "main",
            group: "rel",
            replace: "\u2248"
        },
        "\\cong": {
            font: "main",
            group: "rel",
            replace: "\u2245"
        },
        "\\ge": {
            font: "main",
            group: "rel",
            replace: "\u2265"
        },
        "\\geq": {
            font: "main",
            group: "rel",
            replace: "\u2265"
        },
        "\\gets": {
            font: "main",
            group: "rel",
            replace: "\u2190"
        },
        "\\in": {
            font: "main",
            group: "rel",
            replace: "\u2208"
        },
        "\\notin": {
            font: "main",
            group: "rel",
            replace: "\u2209"
        },
        "\\subset": {
            font: "main",
            group: "rel",
            replace: "\u2282"
        },
        "\\supset": {
            font: "main",
            group: "rel",
            replace: "\u2283"
        },
        "\\subseteq": {
            font: "main",
            group: "rel",
            replace: "\u2286"
        },
        "\\supseteq": {
            font: "main",
            group: "rel",
            replace: "\u2287"
        },
        "\\nsubseteq": {
            font: "ams",
            group: "rel",
            replace: "\u2288"
        },
        "\\nsupseteq": {
            font: "ams",
            group: "rel",
            replace: "\u2289"
        },
        "\\models": {
            font: "main",
            group: "rel",
            replace: "\u22a8"
        },
        "\\leftarrow": {
            font: "main",
            group: "rel",
            replace: "\u2190"
        },
        "\\le": {
            font: "main",
            group: "rel",
            replace: "\u2264"
        },
        "\\leq": {
            font: "main",
            group: "rel",
            replace: "\u2264"
        },
        "\\ne": {
            font: "main",
            group: "rel",
            replace: "\u2260"
        },
        "\\neq": {
            font: "main",
            group: "rel",
            replace: "\u2260"
        },
        "\\rightarrow": {
            font: "main",
            group: "rel",
            replace: "\u2192"
        },
        "\\to": {
            font: "main",
            group: "rel",
            replace: "\u2192"
        },
        "\\ngeq": {
            font: "ams",
            group: "rel",
            replace: "\u2271"
        },
        "\\nleq": {
            font: "ams",
            group: "rel",
            replace: "\u2270"
        },
        "\\!": {
            font: "main",
            group: "spacing"
        },
        "\\ ": {
            font: "main",
            group: "spacing",
            replace: "\u00a0"
        },
        "~": {
            font: "main",
            group: "spacing",
            replace: "\u00a0"
        },
        "\\,": {
            font: "main",
            group: "spacing"
        },
        "\\:": {
            font: "main",
            group: "spacing"
        },
        "\\;": {
            font: "main",
            group: "spacing"
        },
        "\\enspace": {
            font: "main",
            group: "spacing"
        },
        "\\qquad": {
            font: "main",
            group: "spacing"
        },
        "\\quad": {
            font: "main",
            group: "spacing"
        },
        "\\space": {
            font: "main",
            group: "spacing",
            replace: "\u00a0"
        },
        ",": {
            font: "main",
            group: "punct"
        },
        ";": {
            font: "main",
            group: "punct"
        },
        "\\colon": {
            font: "main",
            group: "punct",
            replace: ":"
        },
        "\\barwedge": {
            font: "ams",
            group: "textord",
            replace: "\u22bc"
        },
        "\\veebar": {
            font: "ams",
            group: "textord",
            replace: "\u22bb"
        },
        "\\odot": {
            font: "main",
            group: "textord",
            replace: "\u2299"
        },
        "\\oplus": {
            font: "main",
            group: "textord",
            replace: "\u2295"
        },
        "\\otimes": {
            font: "main",
            group: "textord",
            replace: "\u2297"
        },
        "\\partial":{
            font: "main",
            group: "textord",
            replace: "\u2202"
        },
        "\\oslash": {
            font: "main",
            group: "textord",
            replace: "\u2298"
        },
        "\\circledcirc": {
            font: "ams",
            group: "textord",
            replace: "\u229a"
        },
        "\\boxdot": {
            font: "ams",
            group: "textord",
            replace: "\u22a1"
        },
        "\\bigtriangleup": {
            font: "main",
            group: "textord",
            replace: "\u25b3"
        },
        "\\bigtriangledown": {
            font: "main",
            group: "textord",
            replace: "\u25bd"
        },
        "\\dagger": {
            font: "main",
            group: "textord",
            replace: "\u2020"
        },
        "\\diamond": {
            font: "main",
            group: "textord",
            replace: "\u22c4"
        },
        "\\star": {
            font: "main",
            group: "textord",
            replace: "\u22c6"
        },
        "\\triangleleft": {
            font: "main",
            group: "textord",
            replace: "\u25c3"
        },
        "\\triangleright": {
            font: "main",
            group: "textord",
            replace: "\u25b9"
        },
        "\\{": {
            font: "main",
            group: "open",
            replace: "{"
        },
        "\\}": {
            font: "main",
            group: "close",
            replace: "}"
        },
        "\\lbrace": {
            font: "main",
            group: "open",
            replace: "{"
        },
        "\\rbrace": {
            font: "main",
            group: "close",
            replace: "}"
        },
        "\\lbrack": {
            font: "main",
            group: "open",
            replace: "["
        },
        "\\rbrack": {
            font: "main",
            group: "close",
            replace: "]"
        },
        "\\lfloor": {
            font: "main",
            group: "open",
            replace: "\u230a"
        },
        "\\rfloor": {
            font: "main",
            group: "close",
            replace: "\u230b"
        },
        "\\lceil": {
            font: "main",
            group: "open",
            replace: "\u2308"
        },
        "\\rceil": {
            font: "main",
            group: "close",
            replace: "\u2309"
        },
        "\\backslash": {
            font: "main",
            group: "textord",
            replace: "\\"
        },
        "|": {
            font: "main",
            group: "textord",
            replace: "\u2223"
        },
        "\\vert": {
            font: "main",
            group: "textord",
            replace: "\u2223"
        },
        "\\|": {
            font: "main",
            group: "textord",
            replace: "\u2225"
        },
        "\\Vert": {
            font: "main",
            group: "textord",
            replace: "\u2225"
        },
        "\\uparrow": {
            font: "main",
            group: "textord",
            replace: "\u2191"
        },
        "\\Uparrow": {
            font: "main",
            group: "textord",
            replace: "\u21d1"
        },
        "\\downarrow": {
            font: "main",
            group: "textord",
            replace: "\u2193"
        },
        "\\Downarrow": {
            font: "main",
            group: "textord",
            replace: "\u21d3"
        },
        "\\updownarrow": {
            font: "main",
            group: "textord",
            replace: "\u2195"
        },
        "\\Updownarrow": {
            font: "main",
            group: "textord",
            replace: "\u21d5"
        },
        "\\coprod": {
            font: "math",
            group: "op",
            replace: "\u2210"
        },
        "\\bigvee": {
            font: "math",
            group: "op",
            replace: "\u22c1"
        },
        "\\bigwedge": {
            font: "math",
            group: "op",
            replace: "\u22c0"
        },
        "\\biguplus": {
            font: "math",
            group: "op",
            replace: "\u2a04"
        },
        "\\bigcap": {
            font: "math",
            group: "op",
            replace: "\u22c2"
        },
        "\\bigcup": {
            font: "math",
            group: "op",
            replace: "\u22c3"
        },
        "\\int": {
            font: "math",
            group: "op",
            replace: "\u222b"
        },
        "\\intop": {
            font: "math",
            group: "op",
            replace: "\u222b"
        },
        "\\iint": {
            font: "math",
            group: "op",
            replace: "\u222c"
        },
        "\\iiint": {
            font: "math",
            group: "op",
            replace: "\u222d"
        },
        "\\prod": {
            font: "math",
            group: "op",
            replace: "\u220f"
        },
        "\\sum": {
            font: "math",
            group: "op",
            replace: "\u2211"
        },
        "\\bigotimes": {
            font: "math",
            group: "op",
            replace: "\u2a02"
        },
        "\\bigoplus": {
            font: "math",
            group: "op",
            replace: "\u2a01"
        },
        "\\bigodot": {
            font: "math",
            group: "op",
            replace: "\u2a00"
        },
        "\\oint": {
            font: "math",
            group: "op",
            replace: "\u222e"
        },
        "\\bigsqcup": {
            font: "math",
            group: "op",
            replace: "\u2a06"
        },
        "\\smallint": {
            font: "math",
            group: "op",
            replace: "\u222b"
        },
        "\\ldots": {
            font: "main",
            group: "punct",
            replace: "\u2026"
        },
        "\\cdots": {
            font: "main",
            group: "inner",
            replace: "\u22ef"
        },
        "\\ddots": {
            font: "main",
            group: "inner",
            replace: "\u22f1"
        },
        "\\vdots": {
            font: "main",
            group: "textord",
            replace: "\u22ee"
        },
        "\\acute": {
            font: "main",
            group: "accent",
            replace: "\u00b4"
        },
        "\\grave": {
            font: "main",
            group: "accent",
            replace: "\u0060"
        },
        "\\ddot": {
            font: "main",
            group: "accent",
            replace: "\u00a8"
        },
        "\\tilde": {
            font: "main",
            group: "accent",
            replace: "\u007e"
        },
        "\\bar": {
            font: "main",
            group: "accent",
            replace: "\u00af"
        },
        "\\breve": {
            font: "main",
            group: "accent",
            replace: "\u02d8"
        },
        "\\check": {
            font: "main",
            group: "accent",
            replace: "\u02c7"
        },
        "\\hat": {
            font: "main",
            group: "accent",
            replace: "\u005e"
        },
        "\\vec": {
            font: "main",
            group: "accent",
            replace: "\u20d7"
        },
        "\\dot": {
            font: "main",
            group: "accent",
            replace: "\u02d9"
        }
    },
    "text": {
        "\\ ": {
            font: "main",
            group: "spacing",
            replace: "\u00a0"
        },
        " ": {
            font: "main",
            group: "spacing",
            replace: "\u00a0"
        },
        "~": {
            font: "main",
            group: "spacing",
            replace: "\u00a0"
        }
    }
};

// There are lots of symbols which are the same, so we add them in afterwards.

// All of these are textords in math mode
var mathTextSymbols = "0123456789/@.\"";
for (var i = 0; i < mathTextSymbols.length; i++) {
    var ch = mathTextSymbols.charAt(i);
    symbols.math[ch] = {
        font: "main",
        group: "textord"
    };
}

// All of these are textords in text mode
var textSymbols = "0123456789`!@*()-=+[]'\";:?/.,";
for (var i = 0; i < textSymbols.length; i++) {
    var ch = textSymbols.charAt(i);
    symbols.text[ch] = {
        font: "main",
        group: "textord"
    };
}

// All of these are textords in text mode, and mathords in math mode
var letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
for (var i = 0; i < letters.length; i++) {
    var ch = letters.charAt(i);
    symbols.math[ch] = {
        font: "main",
        group: "mathord"
    };
    symbols.text[ch] = {
        font: "main",
        group: "textord"
    };
}

module.exports = symbols;

},{}],16:[function(require,module,exports){
/**
 * This file contains a list of utility functions which are useful in other
 * files.
 */

/**
 * Provide an `indexOf` function which works in IE8, but defers to native if
 * possible.
 */
var nativeIndexOf = Array.prototype.indexOf;
var indexOf = function(list, elem) {
    if (list == null) {
        return -1;
    }
    if (nativeIndexOf && list.indexOf === nativeIndexOf) {
        return list.indexOf(elem);
    }
    var i = 0, l = list.length;
    for (; i < l; i++) {
        if (list[i] === elem) {
            return i;
        }
    }
    return -1;
};

/**
 * Return whether an element is contained in a list
 */
var contains = function(list, elem) {
    return indexOf(list, elem) !== -1;
};

// hyphenate and escape adapted from Facebook's React under Apache 2 license

var uppercase = /([A-Z])/g;
var hyphenate = function(str) {
    return str.replace(uppercase, "-$1").toLowerCase();
};

var ESCAPE_LOOKUP = {
  "&": "&amp;",
  ">": "&gt;",
  "<": "&lt;",
  "\"": "&quot;",
  "'": "&#x27;"
};

var ESCAPE_REGEX = /[&><"']/g;

function escaper(match) {
  return ESCAPE_LOOKUP[match];
}

/**
 * Escapes text to prevent scripting attacks.
 *
 * @param {*} text Text value to escape.
 * @return {string} An escaped string.
 */
function escape(text) {
  return ("" + text).replace(ESCAPE_REGEX, escaper);
}

/**
 * A function to set the text content of a DOM element in all supported
 * browsers. Note that we don't define this if there is no document.
 */
var setTextContent;
if (typeof document !== "undefined") {
    var testNode = document.createElement("span");
    if ("textContent" in testNode) {
        setTextContent = function(node, text) {
            node.textContent = text;
        };
    } else {
        setTextContent = function(node, text) {
            node.innerText = text;
        };
    }
}

/**
 * A function to clear a node.
 */
function clearNode(node) {
    setTextContent(node, "");
}

module.exports = {
    contains: contains,
    escape: escape,
    hyphenate: hyphenate,
    indexOf: indexOf,
    setTextContent: setTextContent,
    clearNode: clearNode
};

},{}],17:[function(require,module,exports){
(function (global){
/*! Quill Editor v0.19.7
 *  https://quilljs.com/
 *  Copyright (c) 2014, Jason Chen
 *  Copyright (c) 2013, salesforce.com
 */
!function(e){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=e();else if("function"==typeof define&&define.amd)define([],e);else{var f;"undefined"!=typeof window?f=window:"undefined"!=typeof global?f=global:"undefined"!=typeof self&&(f=self),f.Quill=e()}}(function(){var define,module,exports;return (function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(_dereq_,module,exports){
(function (global){
/**
 * @license
 * Lo-Dash 2.4.1 (Custom Build) <http://lodash.com/>
 * Build: `lodash modern include="difference,intersection,last,all,each,invoke,map,reduce,bind,defer,partial,clone,extend,defaults,omit,values,isElement,isEqual,isNumber,isObject,isString,uniqueId" --debug --output .build/lodash.js`
 * Copyright 2012-2013 The Dojo Foundation <http://dojofoundation.org/>
 * Based on Underscore.js 1.5.2 <http://underscorejs.org/LICENSE>
 * Copyright 2009-2013 Jeremy Ashkenas, DocumentCloud and Investigative Reporters & Editors
 * Available under MIT license <http://lodash.com/license>
 */
;(function() {

  /** Used as a safe reference for `undefined` in pre ES5 environments */
  var undefined;

  /** Used to pool arrays and objects used internally */
  var arrayPool = [],
      objectPool = [];

  /** Used to generate unique IDs */
  var idCounter = 0;

  /** Used to prefix keys to avoid issues with `__proto__` and properties on `Object.prototype` */
  var keyPrefix = +new Date + '';

  /** Used as the size when optimizations are enabled for large arrays */
  var largeArraySize = 75;

  /** Used as the max size of the `arrayPool` and `objectPool` */
  var maxPoolSize = 40;

  /** Used to match regexp flags from their coerced string values */
  var reFlags = /\w*$/;

  /** Used to detected named functions */
  var reFuncName = /^\s*function[ \n\r\t]+\w/;

  /** Used to detect functions containing a `this` reference */
  var reThis = /\bthis\b/;

  /** `Object#toString` result shortcuts */
  var argsClass = '[object Arguments]',
      arrayClass = '[object Array]',
      boolClass = '[object Boolean]',
      dateClass = '[object Date]',
      funcClass = '[object Function]',
      numberClass = '[object Number]',
      objectClass = '[object Object]',
      regexpClass = '[object RegExp]',
      stringClass = '[object String]';

  /** Used to identify object classifications that `_.clone` supports */
  var cloneableClasses = {};
  cloneableClasses[funcClass] = false;
  cloneableClasses[argsClass] = cloneableClasses[arrayClass] =
  cloneableClasses[boolClass] = cloneableClasses[dateClass] =
  cloneableClasses[numberClass] = cloneableClasses[objectClass] =
  cloneableClasses[regexpClass] = cloneableClasses[stringClass] = true;

  /** Used as the property descriptor for `__bindData__` */
  var descriptor = {
    'configurable': false,
    'enumerable': false,
    'value': null,
    'writable': false
  };

  /** Used to determine if values are of the language type Object */
  var objectTypes = {
    'boolean': false,
    'function': true,
    'object': true,
    'number': false,
    'string': false,
    'undefined': false
  };

  /** Used as a reference to the global object */
  var root = (objectTypes[typeof window] && window) || this;

  /** Detect free variable `exports` */
  var freeExports = objectTypes[typeof exports] && exports && !exports.nodeType && exports;

  /** Detect free variable `module` */
  var freeModule = objectTypes[typeof module] && module && !module.nodeType && module;

  /** Detect the popular CommonJS extension `module.exports` */
  var moduleExports = freeModule && freeModule.exports === freeExports && freeExports;

  /** Detect free variable `global` from Node.js or Browserified code and use it as `root` */
  var freeGlobal = objectTypes[typeof global] && global;
  if (freeGlobal && (freeGlobal.global === freeGlobal || freeGlobal.window === freeGlobal)) {
    root = freeGlobal;
  }

  /*--------------------------------------------------------------------------*/

  /**
   * The base implementation of `_.indexOf` without support for binary searches
   * or `fromIndex` constraints.
   *
   * @private
   * @param {Array} array The array to search.
   * @param {*} value The value to search for.
   * @param {number} [fromIndex=0] The index to search from.
   * @returns {number} Returns the index of the matched value or `-1`.
   */
  function baseIndexOf(array, value, fromIndex) {
    var index = (fromIndex || 0) - 1,
        length = array ? array.length : 0;

    while (++index < length) {
      if (array[index] === value) {
        return index;
      }
    }
    return -1;
  }

  /**
   * An implementation of `_.contains` for cache objects that mimics the return
   * signature of `_.indexOf` by returning `0` if the value is found, else `-1`.
   *
   * @private
   * @param {Object} cache The cache object to inspect.
   * @param {*} value The value to search for.
   * @returns {number} Returns `0` if `value` is found, else `-1`.
   */
  function cacheIndexOf(cache, value) {
    var type = typeof value;
    cache = cache.cache;

    if (type == 'boolean' || value == null) {
      return cache[value] ? 0 : -1;
    }
    if (type != 'number' && type != 'string') {
      type = 'object';
    }
    var key = type == 'number' ? value : keyPrefix + value;
    cache = (cache = cache[type]) && cache[key];

    return type == 'object'
      ? (cache && baseIndexOf(cache, value) > -1 ? 0 : -1)
      : (cache ? 0 : -1);
  }

  /**
   * Adds a given value to the corresponding cache object.
   *
   * @private
   * @param {*} value The value to add to the cache.
   */
  function cachePush(value) {
    var cache = this.cache,
        type = typeof value;

    if (type == 'boolean' || value == null) {
      cache[value] = true;
    } else {
      if (type != 'number' && type != 'string') {
        type = 'object';
      }
      var key = type == 'number' ? value : keyPrefix + value,
          typeCache = cache[type] || (cache[type] = {});

      if (type == 'object') {
        (typeCache[key] || (typeCache[key] = [])).push(value);
      } else {
        typeCache[key] = true;
      }
    }
  }

  /**
   * Creates a cache object to optimize linear searches of large arrays.
   *
   * @private
   * @param {Array} [array=[]] The array to search.
   * @returns {null|Object} Returns the cache object or `null` if caching should not be used.
   */
  function createCache(array) {
    var index = -1,
        length = array.length,
        first = array[0],
        mid = array[(length / 2) | 0],
        last = array[length - 1];

    if (first && typeof first == 'object' &&
        mid && typeof mid == 'object' && last && typeof last == 'object') {
      return false;
    }
    var cache = getObject();
    cache['false'] = cache['null'] = cache['true'] = cache['undefined'] = false;

    var result = getObject();
    result.array = array;
    result.cache = cache;
    result.push = cachePush;

    while (++index < length) {
      result.push(array[index]);
    }
    return result;
  }

  /**
   * Gets an array from the array pool or creates a new one if the pool is empty.
   *
   * @private
   * @returns {Array} The array from the pool.
   */
  function getArray() {
    return arrayPool.pop() || [];
  }

  /**
   * Gets an object from the object pool or creates a new one if the pool is empty.
   *
   * @private
   * @returns {Object} The object from the pool.
   */
  function getObject() {
    return objectPool.pop() || {
      'array': null,
      'cache': null,
      'false': false,
      'null': false,
      'number': null,
      'object': null,
      'push': null,
      'string': null,
      'true': false,
      'undefined': false
    };
  }

  /**
   * Releases the given array back to the array pool.
   *
   * @private
   * @param {Array} [array] The array to release.
   */
  function releaseArray(array) {
    array.length = 0;
    if (arrayPool.length < maxPoolSize) {
      arrayPool.push(array);
    }
  }

  /**
   * Releases the given object back to the object pool.
   *
   * @private
   * @param {Object} [object] The object to release.
   */
  function releaseObject(object) {
    var cache = object.cache;
    if (cache) {
      releaseObject(cache);
    }
    object.array = object.cache =object.object = object.number = object.string =null;
    if (objectPool.length < maxPoolSize) {
      objectPool.push(object);
    }
  }

  /**
   * Slices the `collection` from the `start` index up to, but not including,
   * the `end` index.
   *
   * Note: This function is used instead of `Array#slice` to support node lists
   * in IE < 9 and to ensure dense arrays are returned.
   *
   * @private
   * @param {Array|Object|string} collection The collection to slice.
   * @param {number} start The start index.
   * @param {number} end The end index.
   * @returns {Array} Returns the new array.
   */
  function slice(array, start, end) {
    start || (start = 0);
    if (typeof end == 'undefined') {
      end = array ? array.length : 0;
    }
    var index = -1,
        length = end - start || 0,
        result = Array(length < 0 ? 0 : length);

    while (++index < length) {
      result[index] = array[start + index];
    }
    return result;
  }

  /*--------------------------------------------------------------------------*/

  /**
   * Used for `Array` method references.
   *
   * Normally `Array.prototype` would suffice, however, using an array literal
   * avoids issues in Narwhal.
   */
  var arrayRef = [];

  /** Used for native method references */
  var objectProto = Object.prototype;

  /** Used to resolve the internal [[Class]] of values */
  var toString = objectProto.toString;

  /** Used to detect if a method is native */
  var reNative = RegExp('^' +
    String(toString)
      .replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
      .replace(/toString| for [^\]]+/g, '.*?') + '$'
  );

  /** Native method shortcuts */
  var fnToString = Function.prototype.toString,
      hasOwnProperty = objectProto.hasOwnProperty,
      push = arrayRef.push,
      unshift = arrayRef.unshift;

  /** Used to set meta data on functions */
  var defineProperty = (function() {
    // IE 8 only accepts DOM elements
    try {
      var o = {},
          func = isNative(func = Object.defineProperty) && func,
          result = func(o, o, o) && func;
    } catch(e) { }
    return result;
  }());

  /* Native method shortcuts for methods with the same name as other `lodash` methods */
  var nativeCreate = isNative(nativeCreate = Object.create) && nativeCreate,
      nativeIsArray = isNative(nativeIsArray = Array.isArray) && nativeIsArray,
      nativeKeys = isNative(nativeKeys = Object.keys) && nativeKeys,
      nativeMax = Math.max;

  /** Used to lookup a built-in constructor by [[Class]] */
  var ctorByClass = {};
  ctorByClass[arrayClass] = Array;
  ctorByClass[boolClass] = Boolean;
  ctorByClass[dateClass] = Date;
  ctorByClass[funcClass] = Function;
  ctorByClass[objectClass] = Object;
  ctorByClass[numberClass] = Number;
  ctorByClass[regexpClass] = RegExp;
  ctorByClass[stringClass] = String;

  /*--------------------------------------------------------------------------*/

  /**
   * Creates a `lodash` object which wraps the given value to enable intuitive
   * method chaining.
   *
   * In addition to Lo-Dash methods, wrappers also have the following `Array` methods:
   * `concat`, `join`, `pop`, `push`, `reverse`, `shift`, `slice`, `sort`, `splice`,
   * and `unshift`
   *
   * Chaining is supported in custom builds as long as the `value` method is
   * implicitly or explicitly included in the build.
   *
   * The chainable wrapper functions are:
   * `after`, `assign`, `bind`, `bindAll`, `bindKey`, `chain`, `compact`,
   * `compose`, `concat`, `countBy`, `create`, `createCallback`, `curry`,
   * `debounce`, `defaults`, `defer`, `delay`, `difference`, `filter`, `flatten`,
   * `forEach`, `forEachRight`, `forIn`, `forInRight`, `forOwn`, `forOwnRight`,
   * `functions`, `groupBy`, `indexBy`, `initial`, `intersection`, `invert`,
   * `invoke`, `keys`, `map`, `max`, `memoize`, `merge`, `min`, `object`, `omit`,
   * `once`, `pairs`, `partial`, `partialRight`, `pick`, `pluck`, `pull`, `push`,
   * `range`, `reject`, `remove`, `rest`, `reverse`, `shuffle`, `slice`, `sort`,
   * `sortBy`, `splice`, `tap`, `throttle`, `times`, `toArray`, `transform`,
   * `union`, `uniq`, `unshift`, `unzip`, `values`, `where`, `without`, `wrap`,
   * and `zip`
   *
   * The non-chainable wrapper functions are:
   * `clone`, `cloneDeep`, `contains`, `escape`, `every`, `find`, `findIndex`,
   * `findKey`, `findLast`, `findLastIndex`, `findLastKey`, `has`, `identity`,
   * `indexOf`, `isArguments`, `isArray`, `isBoolean`, `isDate`, `isElement`,
   * `isEmpty`, `isEqual`, `isFinite`, `isFunction`, `isNaN`, `isNull`, `isNumber`,
   * `isObject`, `isPlainObject`, `isRegExp`, `isString`, `isUndefined`, `join`,
   * `lastIndexOf`, `mixin`, `noConflict`, `parseInt`, `pop`, `random`, `reduce`,
   * `reduceRight`, `result`, `shift`, `size`, `some`, `sortedIndex`, `runInContext`,
   * `template`, `unescape`, `uniqueId`, and `value`
   *
   * The wrapper functions `first` and `last` return wrapped values when `n` is
   * provided, otherwise they return unwrapped values.
   *
   * Explicit chaining can be enabled by using the `_.chain` method.
   *
   * @name _
   * @constructor
   * @category Chaining
   * @param {*} value The value to wrap in a `lodash` instance.
   * @returns {Object} Returns a `lodash` instance.
   * @example
   *
   * var wrapped = _([1, 2, 3]);
   *
   * // returns an unwrapped value
   * wrapped.reduce(function(sum, num) {
   *   return sum + num;
   * });
   * // => 6
   *
   * // returns a wrapped value
   * var squares = wrapped.map(function(num) {
   *   return num * num;
   * });
   *
   * _.isArray(squares);
   * // => false
   *
   * _.isArray(squares.value());
   * // => true
   */
  function lodash() {
    // no operation performed
  }

  /**
   * An object used to flag environments features.
   *
   * @static
   * @memberOf _
   * @type Object
   */
  var support = lodash.support = {};

  /**
   * Detect if functions can be decompiled by `Function#toString`
   * (all but PS3 and older Opera mobile browsers & avoided in Windows 8 apps).
   *
   * @memberOf _.support
   * @type boolean
   */
  support.funcDecomp = !isNative(root.WinRTError) && reThis.test(function() { return this; });

  /**
   * Detect if `Function#name` is supported (all but IE).
   *
   * @memberOf _.support
   * @type boolean
   */
  support.funcNames = typeof Function.name == 'string';

  /*--------------------------------------------------------------------------*/

  /**
   * The base implementation of `_.bind` that creates the bound function and
   * sets its meta data.
   *
   * @private
   * @param {Array} bindData The bind data array.
   * @returns {Function} Returns the new bound function.
   */
  function baseBind(bindData) {
    var func = bindData[0],
        partialArgs = bindData[2],
        thisArg = bindData[4];

    function bound() {
      // `Function#bind` spec
      // http://es5.github.io/#x15.3.4.5
      if (partialArgs) {
        // avoid `arguments` object deoptimizations by using `slice` instead
        // of `Array.prototype.slice.call` and not assigning `arguments` to a
        // variable as a ternary expression
        var args = slice(partialArgs);
        push.apply(args, arguments);
      }
      // mimic the constructor's `return` behavior
      // http://es5.github.io/#x13.2.2
      if (this instanceof bound) {
        // ensure `new bound` is an instance of `func`
        var thisBinding = baseCreate(func.prototype),
            result = func.apply(thisBinding, args || arguments);
        return isObject(result) ? result : thisBinding;
      }
      return func.apply(thisArg, args || arguments);
    }
    setBindData(bound, bindData);
    return bound;
  }

  /**
   * The base implementation of `_.clone` without argument juggling or support
   * for `thisArg` binding.
   *
   * @private
   * @param {*} value The value to clone.
   * @param {boolean} [isDeep=false] Specify a deep clone.
   * @param {Function} [callback] The function to customize cloning values.
   * @param {Array} [stackA=[]] Tracks traversed source objects.
   * @param {Array} [stackB=[]] Associates clones with source counterparts.
   * @returns {*} Returns the cloned value.
   */
  function baseClone(value, isDeep, callback, stackA, stackB) {
    if (callback) {
      var result = callback(value);
      if (typeof result != 'undefined') {
        return result;
      }
    }
    // inspect [[Class]]
    var isObj = isObject(value);
    if (isObj) {
      var className = toString.call(value);
      if (!cloneableClasses[className]) {
        return value;
      }
      var ctor = ctorByClass[className];
      switch (className) {
        case boolClass:
        case dateClass:
          return new ctor(+value);

        case numberClass:
        case stringClass:
          return new ctor(value);

        case regexpClass:
          result = ctor(value.source, reFlags.exec(value));
          result.lastIndex = value.lastIndex;
          return result;
      }
    } else {
      return value;
    }
    var isArr = isArray(value);
    if (isDeep) {
      // check for circular references and return corresponding clone
      var initedStack = !stackA;
      stackA || (stackA = getArray());
      stackB || (stackB = getArray());

      var length = stackA.length;
      while (length--) {
        if (stackA[length] == value) {
          return stackB[length];
        }
      }
      result = isArr ? ctor(value.length) : {};
    }
    else {
      result = isArr ? slice(value) : assign({}, value);
    }
    // add array properties assigned by `RegExp#exec`
    if (isArr) {
      if (hasOwnProperty.call(value, 'index')) {
        result.index = value.index;
      }
      if (hasOwnProperty.call(value, 'input')) {
        result.input = value.input;
      }
    }
    // exit for shallow clone
    if (!isDeep) {
      return result;
    }
    // add the source value to the stack of traversed objects
    // and associate it with its clone
    stackA.push(value);
    stackB.push(result);

    // recursively populate clone (susceptible to call stack limits)
    (isArr ? forEach : forOwn)(value, function(objValue, key) {
      result[key] = baseClone(objValue, isDeep, callback, stackA, stackB);
    });

    if (initedStack) {
      releaseArray(stackA);
      releaseArray(stackB);
    }
    return result;
  }

  /**
   * The base implementation of `_.create` without support for assigning
   * properties to the created object.
   *
   * @private
   * @param {Object} prototype The object to inherit from.
   * @returns {Object} Returns the new object.
   */
  function baseCreate(prototype, properties) {
    return isObject(prototype) ? nativeCreate(prototype) : {};
  }
  // fallback for browsers without `Object.create`
  if (!nativeCreate) {
    baseCreate = (function() {
      function Object() {}
      return function(prototype) {
        if (isObject(prototype)) {
          Object.prototype = prototype;
          var result = new Object;
          Object.prototype = null;
        }
        return result || root.Object();
      };
    }());
  }

  /**
   * The base implementation of `_.createCallback` without support for creating
   * "_.pluck" or "_.where" style callbacks.
   *
   * @private
   * @param {*} [func=identity] The value to convert to a callback.
   * @param {*} [thisArg] The `this` binding of the created callback.
   * @param {number} [argCount] The number of arguments the callback accepts.
   * @returns {Function} Returns a callback function.
   */
  function baseCreateCallback(func, thisArg, argCount) {
    if (typeof func != 'function') {
      return identity;
    }
    // exit early for no `thisArg` or already bound by `Function#bind`
    if (typeof thisArg == 'undefined' || !('prototype' in func)) {
      return func;
    }
    var bindData = func.__bindData__;
    if (typeof bindData == 'undefined') {
      if (support.funcNames) {
        bindData = !func.name;
      }
      bindData = bindData || !support.funcDecomp;
      if (!bindData) {
        var source = fnToString.call(func);
        if (!support.funcNames) {
          bindData = !reFuncName.test(source);
        }
        if (!bindData) {
          // checks if `func` references the `this` keyword and stores the result
          bindData = reThis.test(source);
          setBindData(func, bindData);
        }
      }
    }
    // exit early if there are no `this` references or `func` is bound
    if (bindData === false || (bindData !== true && bindData[1] & 1)) {
      return func;
    }
    switch (argCount) {
      case 1: return function(value) {
        return func.call(thisArg, value);
      };
      case 2: return function(a, b) {
        return func.call(thisArg, a, b);
      };
      case 3: return function(value, index, collection) {
        return func.call(thisArg, value, index, collection);
      };
      case 4: return function(accumulator, value, index, collection) {
        return func.call(thisArg, accumulator, value, index, collection);
      };
    }
    return bind(func, thisArg);
  }

  /**
   * The base implementation of `createWrapper` that creates the wrapper and
   * sets its meta data.
   *
   * @private
   * @param {Array} bindData The bind data array.
   * @returns {Function} Returns the new function.
   */
  function baseCreateWrapper(bindData) {
    var func = bindData[0],
        bitmask = bindData[1],
        partialArgs = bindData[2],
        partialRightArgs = bindData[3],
        thisArg = bindData[4],
        arity = bindData[5];

    var isBind = bitmask & 1,
        isBindKey = bitmask & 2,
        isCurry = bitmask & 4,
        isCurryBound = bitmask & 8,
        key = func;

    function bound() {
      var thisBinding = isBind ? thisArg : this;
      if (partialArgs) {
        var args = slice(partialArgs);
        push.apply(args, arguments);
      }
      if (partialRightArgs || isCurry) {
        args || (args = slice(arguments));
        if (partialRightArgs) {
          push.apply(args, partialRightArgs);
        }
        if (isCurry && args.length < arity) {
          bitmask |= 16 & ~32;
          return baseCreateWrapper([func, (isCurryBound ? bitmask : bitmask & ~3), args, null, thisArg, arity]);
        }
      }
      args || (args = arguments);
      if (isBindKey) {
        func = thisBinding[key];
      }
      if (this instanceof bound) {
        thisBinding = baseCreate(func.prototype);
        var result = func.apply(thisBinding, args);
        return isObject(result) ? result : thisBinding;
      }
      return func.apply(thisBinding, args);
    }
    setBindData(bound, bindData);
    return bound;
  }

  /**
   * The base implementation of `_.difference` that accepts a single array
   * of values to exclude.
   *
   * @private
   * @param {Array} array The array to process.
   * @param {Array} [values] The array of values to exclude.
   * @returns {Array} Returns a new array of filtered values.
   */
  function baseDifference(array, values) {
    var index = -1,
        indexOf = getIndexOf(),
        length = array ? array.length : 0,
        isLarge = length >= largeArraySize && indexOf === baseIndexOf,
        result = [];

    if (isLarge) {
      var cache = createCache(values);
      if (cache) {
        indexOf = cacheIndexOf;
        values = cache;
      } else {
        isLarge = false;
      }
    }
    while (++index < length) {
      var value = array[index];
      if (indexOf(values, value) < 0) {
        result.push(value);
      }
    }
    if (isLarge) {
      releaseObject(values);
    }
    return result;
  }

  /**
   * The base implementation of `_.flatten` without support for callback
   * shorthands or `thisArg` binding.
   *
   * @private
   * @param {Array} array The array to flatten.
   * @param {boolean} [isShallow=false] A flag to restrict flattening to a single level.
   * @param {boolean} [isStrict=false] A flag to restrict flattening to arrays and `arguments` objects.
   * @param {number} [fromIndex=0] The index to start from.
   * @returns {Array} Returns a new flattened array.
   */
  function baseFlatten(array, isShallow, isStrict, fromIndex) {
    var index = (fromIndex || 0) - 1,
        length = array ? array.length : 0,
        result = [];

    while (++index < length) {
      var value = array[index];

      if (value && typeof value == 'object' && typeof value.length == 'number'
          && (isArray(value) || isArguments(value))) {
        // recursively flatten arrays (susceptible to call stack limits)
        if (!isShallow) {
          value = baseFlatten(value, isShallow, isStrict);
        }
        var valIndex = -1,
            valLength = value.length,
            resIndex = result.length;

        result.length += valLength;
        while (++valIndex < valLength) {
          result[resIndex++] = value[valIndex];
        }
      } else if (!isStrict) {
        result.push(value);
      }
    }
    return result;
  }

  /**
   * The base implementation of `_.isEqual`, without support for `thisArg` binding,
   * that allows partial "_.where" style comparisons.
   *
   * @private
   * @param {*} a The value to compare.
   * @param {*} b The other value to compare.
   * @param {Function} [callback] The function to customize comparing values.
   * @param {Function} [isWhere=false] A flag to indicate performing partial comparisons.
   * @param {Array} [stackA=[]] Tracks traversed `a` objects.
   * @param {Array} [stackB=[]] Tracks traversed `b` objects.
   * @returns {boolean} Returns `true` if the values are equivalent, else `false`.
   */
  function baseIsEqual(a, b, callback, isWhere, stackA, stackB) {
    // used to indicate that when comparing objects, `a` has at least the properties of `b`
    if (callback) {
      var result = callback(a, b);
      if (typeof result != 'undefined') {
        return !!result;
      }
    }
    // exit early for identical values
    if (a === b) {
      // treat `+0` vs. `-0` as not equal
      return a !== 0 || (1 / a == 1 / b);
    }
    var type = typeof a,
        otherType = typeof b;

    // exit early for unlike primitive values
    if (a === a &&
        !(a && objectTypes[type]) &&
        !(b && objectTypes[otherType])) {
      return false;
    }
    // exit early for `null` and `undefined` avoiding ES3's Function#call behavior
    // http://es5.github.io/#x15.3.4.4
    if (a == null || b == null) {
      return a === b;
    }
    // compare [[Class]] names
    var className = toString.call(a),
        otherClass = toString.call(b);

    if (className == argsClass) {
      className = objectClass;
    }
    if (otherClass == argsClass) {
      otherClass = objectClass;
    }
    if (className != otherClass) {
      return false;
    }
    switch (className) {
      case boolClass:
      case dateClass:
        // coerce dates and booleans to numbers, dates to milliseconds and booleans
        // to `1` or `0` treating invalid dates coerced to `NaN` as not equal
        return +a == +b;

      case numberClass:
        // treat `NaN` vs. `NaN` as equal
        return (a != +a)
          ? b != +b
          // but treat `+0` vs. `-0` as not equal
          : (a == 0 ? (1 / a == 1 / b) : a == +b);

      case regexpClass:
      case stringClass:
        // coerce regexes to strings (http://es5.github.io/#x15.10.6.4)
        // treat string primitives and their corresponding object instances as equal
        return a == String(b);
    }
    var isArr = className == arrayClass;
    if (!isArr) {
      // unwrap any `lodash` wrapped values
      var aWrapped = hasOwnProperty.call(a, '__wrapped__'),
          bWrapped = hasOwnProperty.call(b, '__wrapped__');

      if (aWrapped || bWrapped) {
        return baseIsEqual(aWrapped ? a.__wrapped__ : a, bWrapped ? b.__wrapped__ : b, callback, isWhere, stackA, stackB);
      }
      // exit for functions and DOM nodes
      if (className != objectClass) {
        return false;
      }
      // in older versions of Opera, `arguments` objects have `Array` constructors
      var ctorA = a.constructor,
          ctorB = b.constructor;

      // non `Object` object instances with different constructors are not equal
      if (ctorA != ctorB &&
            !(isFunction(ctorA) && ctorA instanceof ctorA && isFunction(ctorB) && ctorB instanceof ctorB) &&
            ('constructor' in a && 'constructor' in b)
          ) {
        return false;
      }
    }
    // assume cyclic structures are equal
    // the algorithm for detecting cyclic structures is adapted from ES 5.1
    // section 15.12.3, abstract operation `JO` (http://es5.github.io/#x15.12.3)
    var initedStack = !stackA;
    stackA || (stackA = getArray());
    stackB || (stackB = getArray());

    var length = stackA.length;
    while (length--) {
      if (stackA[length] == a) {
        return stackB[length] == b;
      }
    }
    var size = 0;
    result = true;

    // add `a` and `b` to the stack of traversed objects
    stackA.push(a);
    stackB.push(b);

    // recursively compare objects and arrays (susceptible to call stack limits)
    if (isArr) {
      // compare lengths to determine if a deep comparison is necessary
      length = a.length;
      size = b.length;
      result = size == length;

      if (result || isWhere) {
        // deep compare the contents, ignoring non-numeric properties
        while (size--) {
          var index = length,
              value = b[size];

          if (isWhere) {
            while (index--) {
              if ((result = baseIsEqual(a[index], value, callback, isWhere, stackA, stackB))) {
                break;
              }
            }
          } else if (!(result = baseIsEqual(a[size], value, callback, isWhere, stackA, stackB))) {
            break;
          }
        }
      }
    }
    else {
      // deep compare objects using `forIn`, instead of `forOwn`, to avoid `Object.keys`
      // which, in this case, is more costly
      forIn(b, function(value, key, b) {
        if (hasOwnProperty.call(b, key)) {
          // count the number of properties.
          size++;
          // deep compare each property value.
          return (result = hasOwnProperty.call(a, key) && baseIsEqual(a[key], value, callback, isWhere, stackA, stackB));
        }
      });

      if (result && !isWhere) {
        // ensure both objects have the same number of properties
        forIn(a, function(value, key, a) {
          if (hasOwnProperty.call(a, key)) {
            // `size` will be `-1` if `a` has more properties than `b`
            return (result = --size > -1);
          }
        });
      }
    }
    stackA.pop();
    stackB.pop();

    if (initedStack) {
      releaseArray(stackA);
      releaseArray(stackB);
    }
    return result;
  }

  /**
   * Creates a function that, when called, either curries or invokes `func`
   * with an optional `this` binding and partially applied arguments.
   *
   * @private
   * @param {Function|string} func The function or method name to reference.
   * @param {number} bitmask The bitmask of method flags to compose.
   *  The bitmask may be composed of the following flags:
   *  1 - `_.bind`
   *  2 - `_.bindKey`
   *  4 - `_.curry`
   *  8 - `_.curry` (bound)
   *  16 - `_.partial`
   *  32 - `_.partialRight`
   * @param {Array} [partialArgs] An array of arguments to prepend to those
   *  provided to the new function.
   * @param {Array} [partialRightArgs] An array of arguments to append to those
   *  provided to the new function.
   * @param {*} [thisArg] The `this` binding of `func`.
   * @param {number} [arity] The arity of `func`.
   * @returns {Function} Returns the new function.
   */
  function createWrapper(func, bitmask, partialArgs, partialRightArgs, thisArg, arity) {
    var isBind = bitmask & 1,
        isBindKey = bitmask & 2,
        isCurry = bitmask & 4,
        isCurryBound = bitmask & 8,
        isPartial = bitmask & 16,
        isPartialRight = bitmask & 32;

    if (!isBindKey && !isFunction(func)) {
      throw new TypeError;
    }
    if (isPartial && !partialArgs.length) {
      bitmask &= ~16;
      isPartial = partialArgs = false;
    }
    if (isPartialRight && !partialRightArgs.length) {
      bitmask &= ~32;
      isPartialRight = partialRightArgs = false;
    }
    var bindData = func && func.__bindData__;
    if (bindData && bindData !== true) {
      // clone `bindData`
      bindData = slice(bindData);
      if (bindData[2]) {
        bindData[2] = slice(bindData[2]);
      }
      if (bindData[3]) {
        bindData[3] = slice(bindData[3]);
      }
      // set `thisBinding` is not previously bound
      if (isBind && !(bindData[1] & 1)) {
        bindData[4] = thisArg;
      }
      // set if previously bound but not currently (subsequent curried functions)
      if (!isBind && bindData[1] & 1) {
        bitmask |= 8;
      }
      // set curried arity if not yet set
      if (isCurry && !(bindData[1] & 4)) {
        bindData[5] = arity;
      }
      // append partial left arguments
      if (isPartial) {
        push.apply(bindData[2] || (bindData[2] = []), partialArgs);
      }
      // append partial right arguments
      if (isPartialRight) {
        unshift.apply(bindData[3] || (bindData[3] = []), partialRightArgs);
      }
      // merge flags
      bindData[1] |= bitmask;
      return createWrapper.apply(null, bindData);
    }
    // fast path for `_.bind`
    var creater = (bitmask == 1 || bitmask === 17) ? baseBind : baseCreateWrapper;
    return creater([func, bitmask, partialArgs, partialRightArgs, thisArg, arity]);
  }

  /**
   * Gets the appropriate "indexOf" function. If the `_.indexOf` method is
   * customized, this method returns the custom method, otherwise it returns
   * the `baseIndexOf` function.
   *
   * @private
   * @returns {Function} Returns the "indexOf" function.
   */
  function getIndexOf() {
    var result = (result = lodash.indexOf) === indexOf ? baseIndexOf : result;
    return result;
  }

  /**
   * Checks if `value` is a native function.
   *
   * @private
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if the `value` is a native function, else `false`.
   */
  function isNative(value) {
    return typeof value == 'function' && reNative.test(value);
  }

  /**
   * Sets `this` binding data on a given function.
   *
   * @private
   * @param {Function} func The function to set data on.
   * @param {Array} value The data array to set.
   */
  var setBindData = !defineProperty ? noop : function(func, value) {
    descriptor.value = value;
    defineProperty(func, '__bindData__', descriptor);
  };

  /*--------------------------------------------------------------------------*/

  /**
   * Checks if `value` is an `arguments` object.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if the `value` is an `arguments` object, else `false`.
   * @example
   *
   * (function() { return _.isArguments(arguments); })(1, 2, 3);
   * // => true
   *
   * _.isArguments([1, 2, 3]);
   * // => false
   */
  function isArguments(value) {
    return value && typeof value == 'object' && typeof value.length == 'number' &&
      toString.call(value) == argsClass || false;
  }

  /**
   * Checks if `value` is an array.
   *
   * @static
   * @memberOf _
   * @type Function
   * @category Objects
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if the `value` is an array, else `false`.
   * @example
   *
   * (function() { return _.isArray(arguments); })();
   * // => false
   *
   * _.isArray([1, 2, 3]);
   * // => true
   */
  var isArray = nativeIsArray || function(value) {
    return value && typeof value == 'object' && typeof value.length == 'number' &&
      toString.call(value) == arrayClass || false;
  };

  /**
   * A fallback implementation of `Object.keys` which produces an array of the
   * given object's own enumerable property names.
   *
   * @private
   * @type Function
   * @param {Object} object The object to inspect.
   * @returns {Array} Returns an array of property names.
   */
  var shimKeys = function(object) {
    var index, iterable = object, result = [];
    if (!iterable) return result;
    if (!(objectTypes[typeof object])) return result;
      for (index in iterable) {
        if (hasOwnProperty.call(iterable, index)) {
          result.push(index);
        }
      }
    return result
  };

  /**
   * Creates an array composed of the own enumerable property names of an object.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {Object} object The object to inspect.
   * @returns {Array} Returns an array of property names.
   * @example
   *
   * _.keys({ 'one': 1, 'two': 2, 'three': 3 });
   * // => ['one', 'two', 'three'] (property order is not guaranteed across environments)
   */
  var keys = !nativeKeys ? shimKeys : function(object) {
    if (!isObject(object)) {
      return [];
    }
    return nativeKeys(object);
  };

  /*--------------------------------------------------------------------------*/

  /**
   * Assigns own enumerable properties of source object(s) to the destination
   * object. Subsequent sources will overwrite property assignments of previous
   * sources. If a callback is provided it will be executed to produce the
   * assigned values. The callback is bound to `thisArg` and invoked with two
   * arguments; (objectValue, sourceValue).
   *
   * @static
   * @memberOf _
   * @type Function
   * @alias extend
   * @category Objects
   * @param {Object} object The destination object.
   * @param {...Object} [source] The source objects.
   * @param {Function} [callback] The function to customize assigning values.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {Object} Returns the destination object.
   * @example
   *
   * _.assign({ 'name': 'fred' }, { 'employer': 'slate' });
   * // => { 'name': 'fred', 'employer': 'slate' }
   *
   * var defaults = _.partialRight(_.assign, function(a, b) {
   *   return typeof a == 'undefined' ? b : a;
   * });
   *
   * var object = { 'name': 'barney' };
   * defaults(object, { 'name': 'fred', 'employer': 'slate' });
   * // => { 'name': 'barney', 'employer': 'slate' }
   */
  var assign = function(object, source, guard) {
    var index, iterable = object, result = iterable;
    if (!iterable) return result;
    var args = arguments,
        argsIndex = 0,
        argsLength = typeof guard == 'number' ? 2 : args.length;
    if (argsLength > 3 && typeof args[argsLength - 2] == 'function') {
      var callback = baseCreateCallback(args[--argsLength - 1], args[argsLength--], 2);
    } else if (argsLength > 2 && typeof args[argsLength - 1] == 'function') {
      callback = args[--argsLength];
    }
    while (++argsIndex < argsLength) {
      iterable = args[argsIndex];
      if (iterable && objectTypes[typeof iterable]) {
      var ownIndex = -1,
          ownProps = objectTypes[typeof iterable] && keys(iterable),
          length = ownProps ? ownProps.length : 0;

      while (++ownIndex < length) {
        index = ownProps[ownIndex];
        result[index] = callback ? callback(result[index], iterable[index]) : iterable[index];
      }
      }
    }
    return result
  };

  /**
   * Creates a clone of `value`. If `isDeep` is `true` nested objects will also
   * be cloned, otherwise they will be assigned by reference. If a callback
   * is provided it will be executed to produce the cloned values. If the
   * callback returns `undefined` cloning will be handled by the method instead.
   * The callback is bound to `thisArg` and invoked with one argument; (value).
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {*} value The value to clone.
   * @param {boolean} [isDeep=false] Specify a deep clone.
   * @param {Function} [callback] The function to customize cloning values.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {*} Returns the cloned value.
   * @example
   *
   * var characters = [
   *   { 'name': 'barney', 'age': 36 },
   *   { 'name': 'fred',   'age': 40 }
   * ];
   *
   * var shallow = _.clone(characters);
   * shallow[0] === characters[0];
   * // => true
   *
   * var deep = _.clone(characters, true);
   * deep[0] === characters[0];
   * // => false
   *
   * _.mixin({
   *   'clone': _.partialRight(_.clone, function(value) {
   *     return _.isElement(value) ? value.cloneNode(false) : undefined;
   *   })
   * });
   *
   * var clone = _.clone(document.body);
   * clone.childNodes.length;
   * // => 0
   */
  function clone(value, isDeep, callback, thisArg) {
    // allows working with "Collections" methods without using their `index`
    // and `collection` arguments for `isDeep` and `callback`
    if (typeof isDeep != 'boolean' && isDeep != null) {
      thisArg = callback;
      callback = isDeep;
      isDeep = false;
    }
    return baseClone(value, isDeep, typeof callback == 'function' && baseCreateCallback(callback, thisArg, 1));
  }

  /**
   * Assigns own enumerable properties of source object(s) to the destination
   * object for all destination properties that resolve to `undefined`. Once a
   * property is set, additional defaults of the same property will be ignored.
   *
   * @static
   * @memberOf _
   * @type Function
   * @category Objects
   * @param {Object} object The destination object.
   * @param {...Object} [source] The source objects.
   * @param- {Object} [guard] Allows working with `_.reduce` without using its
   *  `key` and `object` arguments as sources.
   * @returns {Object} Returns the destination object.
   * @example
   *
   * var object = { 'name': 'barney' };
   * _.defaults(object, { 'name': 'fred', 'employer': 'slate' });
   * // => { 'name': 'barney', 'employer': 'slate' }
   */
  var defaults = function(object, source, guard) {
    var index, iterable = object, result = iterable;
    if (!iterable) return result;
    var args = arguments,
        argsIndex = 0,
        argsLength = typeof guard == 'number' ? 2 : args.length;
    while (++argsIndex < argsLength) {
      iterable = args[argsIndex];
      if (iterable && objectTypes[typeof iterable]) {
      var ownIndex = -1,
          ownProps = objectTypes[typeof iterable] && keys(iterable),
          length = ownProps ? ownProps.length : 0;

      while (++ownIndex < length) {
        index = ownProps[ownIndex];
        if (typeof result[index] == 'undefined') result[index] = iterable[index];
      }
      }
    }
    return result
  };

  /**
   * Iterates over own and inherited enumerable properties of an object,
   * executing the callback for each property. The callback is bound to `thisArg`
   * and invoked with three arguments; (value, key, object). Callbacks may exit
   * iteration early by explicitly returning `false`.
   *
   * @static
   * @memberOf _
   * @type Function
   * @category Objects
   * @param {Object} object The object to iterate over.
   * @param {Function} [callback=identity] The function called per iteration.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {Object} Returns `object`.
   * @example
   *
   * function Shape() {
   *   this.x = 0;
   *   this.y = 0;
   * }
   *
   * Shape.prototype.move = function(x, y) {
   *   this.x += x;
   *   this.y += y;
   * };
   *
   * _.forIn(new Shape, function(value, key) {
   *   console.log(key);
   * });
   * // => logs 'x', 'y', and 'move' (property order is not guaranteed across environments)
   */
  var forIn = function(collection, callback, thisArg) {
    var index, iterable = collection, result = iterable;
    if (!iterable) return result;
    if (!objectTypes[typeof iterable]) return result;
    callback = callback && typeof thisArg == 'undefined' ? callback : baseCreateCallback(callback, thisArg, 3);
      for (index in iterable) {
        if (callback(iterable[index], index, collection) === false) return result;
      }
    return result
  };

  /**
   * Iterates over own enumerable properties of an object, executing the callback
   * for each property. The callback is bound to `thisArg` and invoked with three
   * arguments; (value, key, object). Callbacks may exit iteration early by
   * explicitly returning `false`.
   *
   * @static
   * @memberOf _
   * @type Function
   * @category Objects
   * @param {Object} object The object to iterate over.
   * @param {Function} [callback=identity] The function called per iteration.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {Object} Returns `object`.
   * @example
   *
   * _.forOwn({ '0': 'zero', '1': 'one', 'length': 2 }, function(num, key) {
   *   console.log(key);
   * });
   * // => logs '0', '1', and 'length' (property order is not guaranteed across environments)
   */
  var forOwn = function(collection, callback, thisArg) {
    var index, iterable = collection, result = iterable;
    if (!iterable) return result;
    if (!objectTypes[typeof iterable]) return result;
    callback = callback && typeof thisArg == 'undefined' ? callback : baseCreateCallback(callback, thisArg, 3);
      var ownIndex = -1,
          ownProps = objectTypes[typeof iterable] && keys(iterable),
          length = ownProps ? ownProps.length : 0;

      while (++ownIndex < length) {
        index = ownProps[ownIndex];
        if (callback(iterable[index], index, collection) === false) return result;
      }
    return result
  };

  /**
   * Checks if `value` is a DOM element.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if the `value` is a DOM element, else `false`.
   * @example
   *
   * _.isElement(document.body);
   * // => true
   */
  function isElement(value) {
    return value && value.nodeType === 1 || false;
  }

  /**
   * Performs a deep comparison between two values to determine if they are
   * equivalent to each other. If a callback is provided it will be executed
   * to compare values. If the callback returns `undefined` comparisons will
   * be handled by the method instead. The callback is bound to `thisArg` and
   * invoked with two arguments; (a, b).
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {*} a The value to compare.
   * @param {*} b The other value to compare.
   * @param {Function} [callback] The function to customize comparing values.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {boolean} Returns `true` if the values are equivalent, else `false`.
   * @example
   *
   * var object = { 'name': 'fred' };
   * var copy = { 'name': 'fred' };
   *
   * object == copy;
   * // => false
   *
   * _.isEqual(object, copy);
   * // => true
   *
   * var words = ['hello', 'goodbye'];
   * var otherWords = ['hi', 'goodbye'];
   *
   * _.isEqual(words, otherWords, function(a, b) {
   *   var reGreet = /^(?:hello|hi)$/i,
   *       aGreet = _.isString(a) && reGreet.test(a),
   *       bGreet = _.isString(b) && reGreet.test(b);
   *
   *   return (aGreet || bGreet) ? (aGreet == bGreet) : undefined;
   * });
   * // => true
   */
  function isEqual(a, b, callback, thisArg) {
    return baseIsEqual(a, b, typeof callback == 'function' && baseCreateCallback(callback, thisArg, 2));
  }

  /**
   * Checks if `value` is a function.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if the `value` is a function, else `false`.
   * @example
   *
   * _.isFunction(_);
   * // => true
   */
  function isFunction(value) {
    return typeof value == 'function';
  }

  /**
   * Checks if `value` is the language type of Object.
   * (e.g. arrays, functions, objects, regexes, `new Number(0)`, and `new String('')`)
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if the `value` is an object, else `false`.
   * @example
   *
   * _.isObject({});
   * // => true
   *
   * _.isObject([1, 2, 3]);
   * // => true
   *
   * _.isObject(1);
   * // => false
   */
  function isObject(value) {
    // check if the value is the ECMAScript language type of Object
    // http://es5.github.io/#x8
    // and avoid a V8 bug
    // http://code.google.com/p/v8/issues/detail?id=2291
    return !!(value && objectTypes[typeof value]);
  }

  /**
   * Checks if `value` is a number.
   *
   * Note: `NaN` is considered a number. See http://es5.github.io/#x8.5.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if the `value` is a number, else `false`.
   * @example
   *
   * _.isNumber(8.4 * 5);
   * // => true
   */
  function isNumber(value) {
    return typeof value == 'number' ||
      value && typeof value == 'object' && toString.call(value) == numberClass || false;
  }

  /**
   * Checks if `value` is a string.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {*} value The value to check.
   * @returns {boolean} Returns `true` if the `value` is a string, else `false`.
   * @example
   *
   * _.isString('fred');
   * // => true
   */
  function isString(value) {
    return typeof value == 'string' ||
      value && typeof value == 'object' && toString.call(value) == stringClass || false;
  }

  /**
   * Creates a shallow clone of `object` excluding the specified properties.
   * Property names may be specified as individual arguments or as arrays of
   * property names. If a callback is provided it will be executed for each
   * property of `object` omitting the properties the callback returns truey
   * for. The callback is bound to `thisArg` and invoked with three arguments;
   * (value, key, object).
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {Object} object The source object.
   * @param {Function|...string|string[]} [callback] The properties to omit or the
   *  function called per iteration.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {Object} Returns an object without the omitted properties.
   * @example
   *
   * _.omit({ 'name': 'fred', 'age': 40 }, 'age');
   * // => { 'name': 'fred' }
   *
   * _.omit({ 'name': 'fred', 'age': 40 }, function(value) {
   *   return typeof value == 'number';
   * });
   * // => { 'name': 'fred' }
   */
  function omit(object, callback, thisArg) {
    var result = {};
    if (typeof callback != 'function') {
      var props = [];
      forIn(object, function(value, key) {
        props.push(key);
      });
      props = baseDifference(props, baseFlatten(arguments, true, false, 1));

      var index = -1,
          length = props.length;

      while (++index < length) {
        var key = props[index];
        result[key] = object[key];
      }
    } else {
      callback = lodash.createCallback(callback, thisArg, 3);
      forIn(object, function(value, key, object) {
        if (!callback(value, key, object)) {
          result[key] = value;
        }
      });
    }
    return result;
  }

  /**
   * Creates an array composed of the own enumerable property values of `object`.
   *
   * @static
   * @memberOf _
   * @category Objects
   * @param {Object} object The object to inspect.
   * @returns {Array} Returns an array of property values.
   * @example
   *
   * _.values({ 'one': 1, 'two': 2, 'three': 3 });
   * // => [1, 2, 3] (property order is not guaranteed across environments)
   */
  function values(object) {
    var index = -1,
        props = keys(object),
        length = props.length,
        result = Array(length);

    while (++index < length) {
      result[index] = object[props[index]];
    }
    return result;
  }

  /*--------------------------------------------------------------------------*/

  /**
   * Checks if the given callback returns truey value for **all** elements of
   * a collection. The callback is bound to `thisArg` and invoked with three
   * arguments; (value, index|key, collection).
   *
   * If a property name is provided for `callback` the created "_.pluck" style
   * callback will return the property value of the given element.
   *
   * If an object is provided for `callback` the created "_.where" style callback
   * will return `true` for elements that have the properties of the given object,
   * else `false`.
   *
   * @static
   * @memberOf _
   * @alias all
   * @category Collections
   * @param {Array|Object|string} collection The collection to iterate over.
   * @param {Function|Object|string} [callback=identity] The function called
   *  per iteration. If a property name or object is provided it will be used
   *  to create a "_.pluck" or "_.where" style callback, respectively.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {boolean} Returns `true` if all elements passed the callback check,
   *  else `false`.
   * @example
   *
   * _.every([true, 1, null, 'yes']);
   * // => false
   *
   * var characters = [
   *   { 'name': 'barney', 'age': 36 },
   *   { 'name': 'fred',   'age': 40 }
   * ];
   *
   * // using "_.pluck" callback shorthand
   * _.every(characters, 'age');
   * // => true
   *
   * // using "_.where" callback shorthand
   * _.every(characters, { 'age': 36 });
   * // => false
   */
  function every(collection, callback, thisArg) {
    var result = true;
    callback = lodash.createCallback(callback, thisArg, 3);

    var index = -1,
        length = collection ? collection.length : 0;

    if (typeof length == 'number') {
      while (++index < length) {
        if (!(result = !!callback(collection[index], index, collection))) {
          break;
        }
      }
    } else {
      forOwn(collection, function(value, index, collection) {
        return (result = !!callback(value, index, collection));
      });
    }
    return result;
  }

  /**
   * Iterates over elements of a collection, executing the callback for each
   * element. The callback is bound to `thisArg` and invoked with three arguments;
   * (value, index|key, collection). Callbacks may exit iteration early by
   * explicitly returning `false`.
   *
   * Note: As with other "Collections" methods, objects with a `length` property
   * are iterated like arrays. To avoid this behavior `_.forIn` or `_.forOwn`
   * may be used for object iteration.
   *
   * @static
   * @memberOf _
   * @alias each
   * @category Collections
   * @param {Array|Object|string} collection The collection to iterate over.
   * @param {Function} [callback=identity] The function called per iteration.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {Array|Object|string} Returns `collection`.
   * @example
   *
   * _([1, 2, 3]).forEach(function(num) { console.log(num); }).join(',');
   * // => logs each number and returns '1,2,3'
   *
   * _.forEach({ 'one': 1, 'two': 2, 'three': 3 }, function(num) { console.log(num); });
   * // => logs each number and returns the object (property order is not guaranteed across environments)
   */
  function forEach(collection, callback, thisArg) {
    var index = -1,
        length = collection ? collection.length : 0;

    callback = callback && typeof thisArg == 'undefined' ? callback : baseCreateCallback(callback, thisArg, 3);
    if (typeof length == 'number') {
      while (++index < length) {
        if (callback(collection[index], index, collection) === false) {
          break;
        }
      }
    } else {
      forOwn(collection, callback);
    }
    return collection;
  }

  /**
   * Invokes the method named by `methodName` on each element in the `collection`
   * returning an array of the results of each invoked method. Additional arguments
   * will be provided to each invoked method. If `methodName` is a function it
   * will be invoked for, and `this` bound to, each element in the `collection`.
   *
   * @static
   * @memberOf _
   * @category Collections
   * @param {Array|Object|string} collection The collection to iterate over.
   * @param {Function|string} methodName The name of the method to invoke or
   *  the function invoked per iteration.
   * @param {...*} [arg] Arguments to invoke the method with.
   * @returns {Array} Returns a new array of the results of each invoked method.
   * @example
   *
   * _.invoke([[5, 1, 7], [3, 2, 1]], 'sort');
   * // => [[1, 5, 7], [1, 2, 3]]
   *
   * _.invoke([123, 456], String.prototype.split, '');
   * // => [['1', '2', '3'], ['4', '5', '6']]
   */
  function invoke(collection, methodName) {
    var args = slice(arguments, 2),
        index = -1,
        isFunc = typeof methodName == 'function',
        length = collection ? collection.length : 0,
        result = Array(typeof length == 'number' ? length : 0);

    forEach(collection, function(value) {
      result[++index] = (isFunc ? methodName : value[methodName]).apply(value, args);
    });
    return result;
  }

  /**
   * Creates an array of values by running each element in the collection
   * through the callback. The callback is bound to `thisArg` and invoked with
   * three arguments; (value, index|key, collection).
   *
   * If a property name is provided for `callback` the created "_.pluck" style
   * callback will return the property value of the given element.
   *
   * If an object is provided for `callback` the created "_.where" style callback
   * will return `true` for elements that have the properties of the given object,
   * else `false`.
   *
   * @static
   * @memberOf _
   * @alias collect
   * @category Collections
   * @param {Array|Object|string} collection The collection to iterate over.
   * @param {Function|Object|string} [callback=identity] The function called
   *  per iteration. If a property name or object is provided it will be used
   *  to create a "_.pluck" or "_.where" style callback, respectively.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {Array} Returns a new array of the results of each `callback` execution.
   * @example
   *
   * _.map([1, 2, 3], function(num) { return num * 3; });
   * // => [3, 6, 9]
   *
   * _.map({ 'one': 1, 'two': 2, 'three': 3 }, function(num) { return num * 3; });
   * // => [3, 6, 9] (property order is not guaranteed across environments)
   *
   * var characters = [
   *   { 'name': 'barney', 'age': 36 },
   *   { 'name': 'fred',   'age': 40 }
   * ];
   *
   * // using "_.pluck" callback shorthand
   * _.map(characters, 'name');
   * // => ['barney', 'fred']
   */
  function map(collection, callback, thisArg) {
    var index = -1,
        length = collection ? collection.length : 0;

    callback = lodash.createCallback(callback, thisArg, 3);
    if (typeof length == 'number') {
      var result = Array(length);
      while (++index < length) {
        result[index] = callback(collection[index], index, collection);
      }
    } else {
      result = [];
      forOwn(collection, function(value, key, collection) {
        result[++index] = callback(value, key, collection);
      });
    }
    return result;
  }

  /**
   * Reduces a collection to a value which is the accumulated result of running
   * each element in the collection through the callback, where each successive
   * callback execution consumes the return value of the previous execution. If
   * `accumulator` is not provided the first element of the collection will be
   * used as the initial `accumulator` value. The callback is bound to `thisArg`
   * and invoked with four arguments; (accumulator, value, index|key, collection).
   *
   * @static
   * @memberOf _
   * @alias foldl, inject
   * @category Collections
   * @param {Array|Object|string} collection The collection to iterate over.
   * @param {Function} [callback=identity] The function called per iteration.
   * @param {*} [accumulator] Initial value of the accumulator.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {*} Returns the accumulated value.
   * @example
   *
   * var sum = _.reduce([1, 2, 3], function(sum, num) {
   *   return sum + num;
   * });
   * // => 6
   *
   * var mapped = _.reduce({ 'a': 1, 'b': 2, 'c': 3 }, function(result, num, key) {
   *   result[key] = num * 3;
   *   return result;
   * }, {});
   * // => { 'a': 3, 'b': 6, 'c': 9 }
   */
  function reduce(collection, callback, accumulator, thisArg) {
    if (!collection) return accumulator;
    var noaccum = arguments.length < 3;
    callback = lodash.createCallback(callback, thisArg, 4);

    var index = -1,
        length = collection.length;

    if (typeof length == 'number') {
      if (noaccum) {
        accumulator = collection[++index];
      }
      while (++index < length) {
        accumulator = callback(accumulator, collection[index], index, collection);
      }
    } else {
      forOwn(collection, function(value, index, collection) {
        accumulator = noaccum
          ? (noaccum = false, value)
          : callback(accumulator, value, index, collection)
      });
    }
    return accumulator;
  }

  /*--------------------------------------------------------------------------*/

  /**
   * Creates an array excluding all values of the provided arrays using strict
   * equality for comparisons, i.e. `===`.
   *
   * @static
   * @memberOf _
   * @category Arrays
   * @param {Array} array The array to process.
   * @param {...Array} [values] The arrays of values to exclude.
   * @returns {Array} Returns a new array of filtered values.
   * @example
   *
   * _.difference([1, 2, 3, 4, 5], [5, 2, 10]);
   * // => [1, 3, 4]
   */
  function difference(array) {
    return baseDifference(array, baseFlatten(arguments, true, true, 1));
  }

  /**
   * Gets the index at which the first occurrence of `value` is found using
   * strict equality for comparisons, i.e. `===`. If the array is already sorted
   * providing `true` for `fromIndex` will run a faster binary search.
   *
   * @static
   * @memberOf _
   * @category Arrays
   * @param {Array} array The array to search.
   * @param {*} value The value to search for.
   * @param {boolean|number} [fromIndex=0] The index to search from or `true`
   *  to perform a binary search on a sorted array.
   * @returns {number} Returns the index of the matched value or `-1`.
   * @example
   *
   * _.indexOf([1, 2, 3, 1, 2, 3], 2);
   * // => 1
   *
   * _.indexOf([1, 2, 3, 1, 2, 3], 2, 3);
   * // => 4
   *
   * _.indexOf([1, 1, 2, 2, 3, 3], 2, true);
   * // => 2
   */
  function indexOf(array, value, fromIndex) {
    if (typeof fromIndex == 'number') {
      var length = array ? array.length : 0;
      fromIndex = (fromIndex < 0 ? nativeMax(0, length + fromIndex) : fromIndex || 0);
    } else if (fromIndex) {
      var index = sortedIndex(array, value);
      return array[index] === value ? index : -1;
    }
    return baseIndexOf(array, value, fromIndex);
  }

  /**
   * Creates an array of unique values present in all provided arrays using
   * strict equality for comparisons, i.e. `===`.
   *
   * @static
   * @memberOf _
   * @category Arrays
   * @param {...Array} [array] The arrays to inspect.
   * @returns {Array} Returns an array of shared values.
   * @example
   *
   * _.intersection([1, 2, 3], [5, 2, 1, 4], [2, 1]);
   * // => [1, 2]
   */
  function intersection() {
    var args = [],
        argsIndex = -1,
        argsLength = arguments.length,
        caches = getArray(),
        indexOf = getIndexOf(),
        trustIndexOf = indexOf === baseIndexOf,
        seen = getArray();

    while (++argsIndex < argsLength) {
      var value = arguments[argsIndex];
      if (isArray(value) || isArguments(value)) {
        args.push(value);
        caches.push(trustIndexOf && value.length >= largeArraySize &&
          createCache(argsIndex ? args[argsIndex] : seen));
      }
    }
    var array = args[0],
        index = -1,
        length = array ? array.length : 0,
        result = [];

    outer:
    while (++index < length) {
      var cache = caches[0];
      value = array[index];

      if ((cache ? cacheIndexOf(cache, value) : indexOf(seen, value)) < 0) {
        argsIndex = argsLength;
        (cache || seen).push(value);
        while (--argsIndex) {
          cache = caches[argsIndex];
          if ((cache ? cacheIndexOf(cache, value) : indexOf(args[argsIndex], value)) < 0) {
            continue outer;
          }
        }
        result.push(value);
      }
    }
    while (argsLength--) {
      cache = caches[argsLength];
      if (cache) {
        releaseObject(cache);
      }
    }
    releaseArray(caches);
    releaseArray(seen);
    return result;
  }

  /**
   * Gets the last element or last `n` elements of an array. If a callback is
   * provided elements at the end of the array are returned as long as the
   * callback returns truey. The callback is bound to `thisArg` and invoked
   * with three arguments; (value, index, array).
   *
   * If a property name is provided for `callback` the created "_.pluck" style
   * callback will return the property value of the given element.
   *
   * If an object is provided for `callback` the created "_.where" style callback
   * will return `true` for elements that have the properties of the given object,
   * else `false`.
   *
   * @static
   * @memberOf _
   * @category Arrays
   * @param {Array} array The array to query.
   * @param {Function|Object|number|string} [callback] The function called
   *  per element or the number of elements to return. If a property name or
   *  object is provided it will be used to create a "_.pluck" or "_.where"
   *  style callback, respectively.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {*} Returns the last element(s) of `array`.
   * @example
   *
   * _.last([1, 2, 3]);
   * // => 3
   *
   * _.last([1, 2, 3], 2);
   * // => [2, 3]
   *
   * _.last([1, 2, 3], function(num) {
   *   return num > 1;
   * });
   * // => [2, 3]
   *
   * var characters = [
   *   { 'name': 'barney',  'blocked': false, 'employer': 'slate' },
   *   { 'name': 'fred',    'blocked': true,  'employer': 'slate' },
   *   { 'name': 'pebbles', 'blocked': true,  'employer': 'na' }
   * ];
   *
   * // using "_.pluck" callback shorthand
   * _.pluck(_.last(characters, 'blocked'), 'name');
   * // => ['fred', 'pebbles']
   *
   * // using "_.where" callback shorthand
   * _.last(characters, { 'employer': 'na' });
   * // => [{ 'name': 'pebbles', 'blocked': true, 'employer': 'na' }]
   */
  function last(array, callback, thisArg) {
    var n = 0,
        length = array ? array.length : 0;

    if (typeof callback != 'number' && callback != null) {
      var index = length;
      callback = lodash.createCallback(callback, thisArg, 3);
      while (index-- && callback(array[index], index, array)) {
        n++;
      }
    } else {
      n = callback;
      if (n == null || thisArg) {
        return array ? array[length - 1] : undefined;
      }
    }
    return slice(array, nativeMax(0, length - n));
  }

  /**
   * Uses a binary search to determine the smallest index at which a value
   * should be inserted into a given sorted array in order to maintain the sort
   * order of the array. If a callback is provided it will be executed for
   * `value` and each element of `array` to compute their sort ranking. The
   * callback is bound to `thisArg` and invoked with one argument; (value).
   *
   * If a property name is provided for `callback` the created "_.pluck" style
   * callback will return the property value of the given element.
   *
   * If an object is provided for `callback` the created "_.where" style callback
   * will return `true` for elements that have the properties of the given object,
   * else `false`.
   *
   * @static
   * @memberOf _
   * @category Arrays
   * @param {Array} array The array to inspect.
   * @param {*} value The value to evaluate.
   * @param {Function|Object|string} [callback=identity] The function called
   *  per iteration. If a property name or object is provided it will be used
   *  to create a "_.pluck" or "_.where" style callback, respectively.
   * @param {*} [thisArg] The `this` binding of `callback`.
   * @returns {number} Returns the index at which `value` should be inserted
   *  into `array`.
   * @example
   *
   * _.sortedIndex([20, 30, 50], 40);
   * // => 2
   *
   * // using "_.pluck" callback shorthand
   * _.sortedIndex([{ 'x': 20 }, { 'x': 30 }, { 'x': 50 }], { 'x': 40 }, 'x');
   * // => 2
   *
   * var dict = {
   *   'wordToNumber': { 'twenty': 20, 'thirty': 30, 'fourty': 40, 'fifty': 50 }
   * };
   *
   * _.sortedIndex(['twenty', 'thirty', 'fifty'], 'fourty', function(word) {
   *   return dict.wordToNumber[word];
   * });
   * // => 2
   *
   * _.sortedIndex(['twenty', 'thirty', 'fifty'], 'fourty', function(word) {
   *   return this.wordToNumber[word];
   * }, dict);
   * // => 2
   */
  function sortedIndex(array, value, callback, thisArg) {
    var low = 0,
        high = array ? array.length : low;

    // explicitly reference `identity` for better inlining in Firefox
    callback = callback ? lodash.createCallback(callback, thisArg, 1) : identity;
    value = callback(value);

    while (low < high) {
      var mid = (low + high) >>> 1;
      (callback(array[mid]) < value)
        ? low = mid + 1
        : high = mid;
    }
    return low;
  }

  /*--------------------------------------------------------------------------*/

  /**
   * Creates a function that, when called, invokes `func` with the `this`
   * binding of `thisArg` and prepends any additional `bind` arguments to those
   * provided to the bound function.
   *
   * @static
   * @memberOf _
   * @category Functions
   * @param {Function} func The function to bind.
   * @param {*} [thisArg] The `this` binding of `func`.
   * @param {...*} [arg] Arguments to be partially applied.
   * @returns {Function} Returns the new bound function.
   * @example
   *
   * var func = function(greeting) {
   *   return greeting + ' ' + this.name;
   * };
   *
   * func = _.bind(func, { 'name': 'fred' }, 'hi');
   * func();
   * // => 'hi fred'
   */
  function bind(func, thisArg) {
    return arguments.length > 2
      ? createWrapper(func, 17, slice(arguments, 2), null, thisArg)
      : createWrapper(func, 1, null, null, thisArg);
  }

  /**
   * Defers executing the `func` function until the current call stack has cleared.
   * Additional arguments will be provided to `func` when it is invoked.
   *
   * @static
   * @memberOf _
   * @category Functions
   * @param {Function} func The function to defer.
   * @param {...*} [arg] Arguments to invoke the function with.
   * @returns {number} Returns the timer id.
   * @example
   *
   * _.defer(function(text) { console.log(text); }, 'deferred');
   * // logs 'deferred' after one or more milliseconds
   */
  function defer(func) {
    if (!isFunction(func)) {
      throw new TypeError;
    }
    var args = slice(arguments, 1);
    return setTimeout(function() { func.apply(undefined, args); }, 1);
  }

  /**
   * Creates a function that, when called, invokes `func` with any additional
   * `partial` arguments prepended to those provided to the new function. This
   * method is similar to `_.bind` except it does **not** alter the `this` binding.
   *
   * @static
   * @memberOf _
   * @category Functions
   * @param {Function} func The function to partially apply arguments to.
   * @param {...*} [arg] Arguments to be partially applied.
   * @returns {Function} Returns the new partially applied function.
   * @example
   *
   * var greet = function(greeting, name) { return greeting + ' ' + name; };
   * var hi = _.partial(greet, 'hi');
   * hi('fred');
   * // => 'hi fred'
   */
  function partial(func) {
    return createWrapper(func, 16, slice(arguments, 1));
  }

  /*--------------------------------------------------------------------------*/

  /**
   * Produces a callback bound to an optional `thisArg`. If `func` is a property
   * name the created callback will return the property value for a given element.
   * If `func` is an object the created callback will return `true` for elements
   * that contain the equivalent object properties, otherwise it will return `false`.
   *
   * @static
   * @memberOf _
   * @category Utilities
   * @param {*} [func=identity] The value to convert to a callback.
   * @param {*} [thisArg] The `this` binding of the created callback.
   * @param {number} [argCount] The number of arguments the callback accepts.
   * @returns {Function} Returns a callback function.
   * @example
   *
   * var characters = [
   *   { 'name': 'barney', 'age': 36 },
   *   { 'name': 'fred',   'age': 40 }
   * ];
   *
   * // wrap to create custom callback shorthands
   * _.createCallback = _.wrap(_.createCallback, function(func, callback, thisArg) {
   *   var match = /^(.+?)__([gl]t)(.+)$/.exec(callback);
   *   return !match ? func(callback, thisArg) : function(object) {
   *     return match[2] == 'gt' ? object[match[1]] > match[3] : object[match[1]] < match[3];
   *   };
   * });
   *
   * _.filter(characters, 'age__gt38');
   * // => [{ 'name': 'fred', 'age': 40 }]
   */
  function createCallback(func, thisArg, argCount) {
    var type = typeof func;
    if (func == null || type == 'function') {
      return baseCreateCallback(func, thisArg, argCount);
    }
    // handle "_.pluck" style callback shorthands
    if (type != 'object') {
      return property(func);
    }
    var props = keys(func),
        key = props[0],
        a = func[key];

    // handle "_.where" style callback shorthands
    if (props.length == 1 && a === a && !isObject(a)) {
      // fast path the common case of providing an object with a single
      // property containing a primitive value
      return function(object) {
        var b = object[key];
        return a === b && (a !== 0 || (1 / a == 1 / b));
      };
    }
    return function(object) {
      var length = props.length,
          result = false;

      while (length--) {
        if (!(result = baseIsEqual(object[props[length]], func[props[length]], null, true))) {
          break;
        }
      }
      return result;
    };
  }

  /**
   * This method returns the first argument provided to it.
   *
   * @static
   * @memberOf _
   * @category Utilities
   * @param {*} value Any value.
   * @returns {*} Returns `value`.
   * @example
   *
   * var object = { 'name': 'fred' };
   * _.identity(object) === object;
   * // => true
   */
  function identity(value) {
    return value;
  }

  /**
   * A no-operation function.
   *
   * @static
   * @memberOf _
   * @category Utilities
   * @example
   *
   * var object = { 'name': 'fred' };
   * _.noop(object) === undefined;
   * // => true
   */
  function noop() {
    // no operation performed
  }

  /**
   * Creates a "_.pluck" style function, which returns the `key` value of a
   * given object.
   *
   * @static
   * @memberOf _
   * @category Utilities
   * @param {string} key The name of the property to retrieve.
   * @returns {Function} Returns the new function.
   * @example
   *
   * var characters = [
   *   { 'name': 'fred',   'age': 40 },
   *   { 'name': 'barney', 'age': 36 }
   * ];
   *
   * var getName = _.property('name');
   *
   * _.map(characters, getName);
   * // => ['barney', 'fred']
   *
   * _.sortBy(characters, getName);
   * // => [{ 'name': 'barney', 'age': 36 }, { 'name': 'fred',   'age': 40 }]
   */
  function property(key) {
    return function(object) {
      return object[key];
    };
  }

  /**
   * Generates a unique ID. If `prefix` is provided the ID will be appended to it.
   *
   * @static
   * @memberOf _
   * @category Utilities
   * @param {string} [prefix] The value to prefix the ID with.
   * @returns {string} Returns the unique ID.
   * @example
   *
   * _.uniqueId('contact_');
   * // => 'contact_104'
   *
   * _.uniqueId();
   * // => '105'
   */
  function uniqueId(prefix) {
    var id = ++idCounter;
    return String(prefix == null ? '' : prefix) + id;
  }

  /*--------------------------------------------------------------------------*/

  lodash.assign = assign;
  lodash.bind = bind;
  lodash.createCallback = createCallback;
  lodash.defaults = defaults;
  lodash.defer = defer;
  lodash.difference = difference;
  lodash.forEach = forEach;
  lodash.forIn = forIn;
  lodash.forOwn = forOwn;
  lodash.intersection = intersection;
  lodash.invoke = invoke;
  lodash.keys = keys;
  lodash.map = map;
  lodash.omit = omit;
  lodash.partial = partial;
  lodash.property = property;
  lodash.values = values;

  // add aliases
  lodash.collect = map;
  lodash.each = forEach;
  lodash.extend = assign;

  /*--------------------------------------------------------------------------*/

  // add functions that return unwrapped values when chaining
  lodash.clone = clone;
  lodash.every = every;
  lodash.identity = identity;
  lodash.indexOf = indexOf;
  lodash.isArguments = isArguments;
  lodash.isArray = isArray;
  lodash.isElement = isElement;
  lodash.isEqual = isEqual;
  lodash.isFunction = isFunction;
  lodash.isNumber = isNumber;
  lodash.isObject = isObject;
  lodash.isString = isString;
  lodash.noop = noop;
  lodash.reduce = reduce;
  lodash.sortedIndex = sortedIndex;
  lodash.uniqueId = uniqueId;

  // add aliases
  lodash.all = every;
  lodash.foldl = reduce;
  lodash.inject = reduce;

  /*--------------------------------------------------------------------------*/

  lodash.last = last;

  /*--------------------------------------------------------------------------*/

  /**
   * The semantic version number.
   *
   * @static
   * @memberOf _
   * @type string
   */
  lodash.VERSION = '2.4.1';

  /*--------------------------------------------------------------------------*/

  // some AMD build optimizers like r.js check for condition patterns like the following:
  if (typeof define == 'function' && typeof define.amd == 'object' && define.amd) {
    // Expose Lo-Dash to the global object even when an AMD loader is present in
    // case Lo-Dash is loaded with a RequireJS shim config.
    // See http://requirejs.org/docs/api.html#config-shim
    root._ = lodash;

    // define as an anonymous module so, through path mapping, it can be
    // referenced as the "underscore" module
    define(function() {
      return lodash;
    });
  }
  // check for `exports` after `define` in case a build optimizer adds an `exports` object
  else if (freeExports && freeModule) {
    // in Node.js or RingoJS
    if (moduleExports) {
      (freeModule.exports = lodash)._ = lodash;
    }
    // in Narwhal or Rhino -require
    else {
      freeExports._ = lodash;
    }
  }
  else {
    // in a browser or Rhino
    root._ = lodash;
  }
}.call(this));

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],2:[function(_dereq_,module,exports){
/*!
 * EventEmitter2
 * https://github.com/hij1nx/EventEmitter2
 *
 * Copyright (c) 2013 hij1nx
 * Licensed under the MIT license.
 */
;!function(undefined) {

  var isArray = Array.isArray ? Array.isArray : function _isArray(obj) {
    return Object.prototype.toString.call(obj) === "[object Array]";
  };
  var defaultMaxListeners = 10;

  function init() {
    this._events = {};
    if (this._conf) {
      configure.call(this, this._conf);
    }
  }

  function configure(conf) {
    if (conf) {

      this._conf = conf;

      conf.delimiter && (this.delimiter = conf.delimiter);
      conf.maxListeners && (this._events.maxListeners = conf.maxListeners);
      conf.wildcard && (this.wildcard = conf.wildcard);
      conf.newListener && (this.newListener = conf.newListener);

      if (this.wildcard) {
        this.listenerTree = {};
      }
    }
  }

  function EventEmitter(conf) {
    this._events = {};
    this.newListener = false;
    configure.call(this, conf);
  }

  //
  // Attention, function return type now is array, always !
  // It has zero elements if no any matches found and one or more
  // elements (leafs) if there are matches
  //
  function searchListenerTree(handlers, type, tree, i) {
    if (!tree) {
      return [];
    }
    var listeners=[], leaf, len, branch, xTree, xxTree, isolatedBranch, endReached,
        typeLength = type.length, currentType = type[i], nextType = type[i+1];
    if (i === typeLength && tree._listeners) {
      //
      // If at the end of the event(s) list and the tree has listeners
      // invoke those listeners.
      //
      if (typeof tree._listeners === 'function') {
        handlers && handlers.push(tree._listeners);
        return [tree];
      } else {
        for (leaf = 0, len = tree._listeners.length; leaf < len; leaf++) {
          handlers && handlers.push(tree._listeners[leaf]);
        }
        return [tree];
      }
    }

    if ((currentType === '*' || currentType === '**') || tree[currentType]) {
      //
      // If the event emitted is '*' at this part
      // or there is a concrete match at this patch
      //
      if (currentType === '*') {
        for (branch in tree) {
          if (branch !== '_listeners' && tree.hasOwnProperty(branch)) {
            listeners = listeners.concat(searchListenerTree(handlers, type, tree[branch], i+1));
          }
        }
        return listeners;
      } else if(currentType === '**') {
        endReached = (i+1 === typeLength || (i+2 === typeLength && nextType === '*'));
        if(endReached && tree._listeners) {
          // The next element has a _listeners, add it to the handlers.
          listeners = listeners.concat(searchListenerTree(handlers, type, tree, typeLength));
        }

        for (branch in tree) {
          if (branch !== '_listeners' && tree.hasOwnProperty(branch)) {
            if(branch === '*' || branch === '**') {
              if(tree[branch]._listeners && !endReached) {
                listeners = listeners.concat(searchListenerTree(handlers, type, tree[branch], typeLength));
              }
              listeners = listeners.concat(searchListenerTree(handlers, type, tree[branch], i));
            } else if(branch === nextType) {
              listeners = listeners.concat(searchListenerTree(handlers, type, tree[branch], i+2));
            } else {
              // No match on this one, shift into the tree but not in the type array.
              listeners = listeners.concat(searchListenerTree(handlers, type, tree[branch], i));
            }
          }
        }
        return listeners;
      }

      listeners = listeners.concat(searchListenerTree(handlers, type, tree[currentType], i+1));
    }

    xTree = tree['*'];
    if (xTree) {
      //
      // If the listener tree will allow any match for this part,
      // then recursively explore all branches of the tree
      //
      searchListenerTree(handlers, type, xTree, i+1);
    }

    xxTree = tree['**'];
    if(xxTree) {
      if(i < typeLength) {
        if(xxTree._listeners) {
          // If we have a listener on a '**', it will catch all, so add its handler.
          searchListenerTree(handlers, type, xxTree, typeLength);
        }

        // Build arrays of matching next branches and others.
        for(branch in xxTree) {
          if(branch !== '_listeners' && xxTree.hasOwnProperty(branch)) {
            if(branch === nextType) {
              // We know the next element will match, so jump twice.
              searchListenerTree(handlers, type, xxTree[branch], i+2);
            } else if(branch === currentType) {
              // Current node matches, move into the tree.
              searchListenerTree(handlers, type, xxTree[branch], i+1);
            } else {
              isolatedBranch = {};
              isolatedBranch[branch] = xxTree[branch];
              searchListenerTree(handlers, type, { '**': isolatedBranch }, i+1);
            }
          }
        }
      } else if(xxTree._listeners) {
        // We have reached the end and still on a '**'
        searchListenerTree(handlers, type, xxTree, typeLength);
      } else if(xxTree['*'] && xxTree['*']._listeners) {
        searchListenerTree(handlers, type, xxTree['*'], typeLength);
      }
    }

    return listeners;
  }

  function growListenerTree(type, listener) {

    type = typeof type === 'string' ? type.split(this.delimiter) : type.slice();

    //
    // Looks for two consecutive '**', if so, don't add the event at all.
    //
    for(var i = 0, len = type.length; i+1 < len; i++) {
      if(type[i] === '**' && type[i+1] === '**') {
        return;
      }
    }

    var tree = this.listenerTree;
    var name = type.shift();

    while (name) {

      if (!tree[name]) {
        tree[name] = {};
      }

      tree = tree[name];

      if (type.length === 0) {

        if (!tree._listeners) {
          tree._listeners = listener;
        }
        else if(typeof tree._listeners === 'function') {
          tree._listeners = [tree._listeners, listener];
        }
        else if (isArray(tree._listeners)) {

          tree._listeners.push(listener);

          if (!tree._listeners.warned) {

            var m = defaultMaxListeners;

            if (typeof this._events.maxListeners !== 'undefined') {
              m = this._events.maxListeners;
            }

            if (m > 0 && tree._listeners.length > m) {

              tree._listeners.warned = true;
              console.error('(node) warning: possible EventEmitter memory ' +
                            'leak detected. %d listeners added. ' +
                            'Use emitter.setMaxListeners() to increase limit.',
                            tree._listeners.length);
              console.trace();
            }
          }
        }
        return true;
      }
      name = type.shift();
    }
    return true;
  }

  // By default EventEmitters will print a warning if more than
  // 10 listeners are added to it. This is a useful default which
  // helps finding memory leaks.
  //
  // Obviously not all Emitters should be limited to 10. This function allows
  // that to be increased. Set to zero for unlimited.

  EventEmitter.prototype.delimiter = '.';

  EventEmitter.prototype.setMaxListeners = function(n) {
    this._events || init.call(this);
    this._events.maxListeners = n;
    if (!this._conf) this._conf = {};
    this._conf.maxListeners = n;
  };

  EventEmitter.prototype.event = '';

  EventEmitter.prototype.once = function(event, fn) {
    this.many(event, 1, fn);
    return this;
  };

  EventEmitter.prototype.many = function(event, ttl, fn) {
    var self = this;

    if (typeof fn !== 'function') {
      throw new Error('many only accepts instances of Function');
    }

    function listener() {
      if (--ttl === 0) {
        self.off(event, listener);
      }
      fn.apply(this, arguments);
    }

    listener._origin = fn;

    this.on(event, listener);

    return self;
  };

  EventEmitter.prototype.emit = function() {

    this._events || init.call(this);

    var type = arguments[0];

    if (type === 'newListener' && !this.newListener) {
      if (!this._events.newListener) { return false; }
    }

    // Loop through the *_all* functions and invoke them.
    if (this._all) {
      var l = arguments.length;
      var args = new Array(l - 1);
      for (var i = 1; i < l; i++) args[i - 1] = arguments[i];
      for (i = 0, l = this._all.length; i < l; i++) {
        this.event = type;
        this._all[i].apply(this, args);
      }
    }

    // If there is no 'error' event listener then throw.
    if (type === 'error') {

      if (!this._all &&
        !this._events.error &&
        !(this.wildcard && this.listenerTree.error)) {

        if (arguments[1] instanceof Error) {
          throw arguments[1]; // Unhandled 'error' event
        } else {
          throw new Error("Uncaught, unspecified 'error' event.");
        }
        return false;
      }
    }

    var handler;

    if(this.wildcard) {
      handler = [];
      var ns = typeof type === 'string' ? type.split(this.delimiter) : type.slice();
      searchListenerTree.call(this, handler, ns, this.listenerTree, 0);
    }
    else {
      handler = this._events[type];
    }

    if (typeof handler === 'function') {
      this.event = type;
      if (arguments.length === 1) {
        handler.call(this);
      }
      else if (arguments.length > 1)
        switch (arguments.length) {
          case 2:
            handler.call(this, arguments[1]);
            break;
          case 3:
            handler.call(this, arguments[1], arguments[2]);
            break;
          // slower
          default:
            var l = arguments.length;
            var args = new Array(l - 1);
            for (var i = 1; i < l; i++) args[i - 1] = arguments[i];
            handler.apply(this, args);
        }
      return true;
    }
    else if (handler) {
      var l = arguments.length;
      var args = new Array(l - 1);
      for (var i = 1; i < l; i++) args[i - 1] = arguments[i];

      var listeners = handler.slice();
      for (var i = 0, l = listeners.length; i < l; i++) {
        this.event = type;
        listeners[i].apply(this, args);
      }
      return (listeners.length > 0) || !!this._all;
    }
    else {
      return !!this._all;
    }

  };

  EventEmitter.prototype.on = function(type, listener) {

    if (typeof type === 'function') {
      this.onAny(type);
      return this;
    }

    if (typeof listener !== 'function') {
      throw new Error('on only accepts instances of Function');
    }
    this._events || init.call(this);

    // To avoid recursion in the case that type == "newListeners"! Before
    // adding it to the listeners, first emit "newListeners".
    this.emit('newListener', type, listener);

    if(this.wildcard) {
      growListenerTree.call(this, type, listener);
      return this;
    }

    if (!this._events[type]) {
      // Optimize the case of one listener. Don't need the extra array object.
      this._events[type] = listener;
    }
    else if(typeof this._events[type] === 'function') {
      // Adding the second element, need to change to array.
      this._events[type] = [this._events[type], listener];
    }
    else if (isArray(this._events[type])) {
      // If we've already got an array, just append.
      this._events[type].push(listener);

      // Check for listener leak
      if (!this._events[type].warned) {

        var m = defaultMaxListeners;

        if (typeof this._events.maxListeners !== 'undefined') {
          m = this._events.maxListeners;
        }

        if (m > 0 && this._events[type].length > m) {

          this._events[type].warned = true;
          console.error('(node) warning: possible EventEmitter memory ' +
                        'leak detected. %d listeners added. ' +
                        'Use emitter.setMaxListeners() to increase limit.',
                        this._events[type].length);
          console.trace();
        }
      }
    }
    return this;
  };

  EventEmitter.prototype.onAny = function(fn) {

    if (typeof fn !== 'function') {
      throw new Error('onAny only accepts instances of Function');
    }

    if(!this._all) {
      this._all = [];
    }

    // Add the function to the event listener collection.
    this._all.push(fn);
    return this;
  };

  EventEmitter.prototype.addListener = EventEmitter.prototype.on;

  EventEmitter.prototype.off = function(type, listener) {
    if (typeof listener !== 'function') {
      throw new Error('removeListener only takes instances of Function');
    }

    var handlers,leafs=[];

    if(this.wildcard) {
      var ns = typeof type === 'string' ? type.split(this.delimiter) : type.slice();
      leafs = searchListenerTree.call(this, null, ns, this.listenerTree, 0);
    }
    else {
      // does not use listeners(), so no side effect of creating _events[type]
      if (!this._events[type]) return this;
      handlers = this._events[type];
      leafs.push({_listeners:handlers});
    }

    for (var iLeaf=0; iLeaf<leafs.length; iLeaf++) {
      var leaf = leafs[iLeaf];
      handlers = leaf._listeners;
      if (isArray(handlers)) {

        var position = -1;

        for (var i = 0, length = handlers.length; i < length; i++) {
          if (handlers[i] === listener ||
            (handlers[i].listener && handlers[i].listener === listener) ||
            (handlers[i]._origin && handlers[i]._origin === listener)) {
            position = i;
            break;
          }
        }

        if (position < 0) {
          continue;
        }

        if(this.wildcard) {
          leaf._listeners.splice(position, 1);
        }
        else {
          this._events[type].splice(position, 1);
        }

        if (handlers.length === 0) {
          if(this.wildcard) {
            delete leaf._listeners;
          }
          else {
            delete this._events[type];
          }
        }
        return this;
      }
      else if (handlers === listener ||
        (handlers.listener && handlers.listener === listener) ||
        (handlers._origin && handlers._origin === listener)) {
        if(this.wildcard) {
          delete leaf._listeners;
        }
        else {
          delete this._events[type];
        }
      }
    }

    return this;
  };

  EventEmitter.prototype.offAny = function(fn) {
    var i = 0, l = 0, fns;
    if (fn && this._all && this._all.length > 0) {
      fns = this._all;
      for(i = 0, l = fns.length; i < l; i++) {
        if(fn === fns[i]) {
          fns.splice(i, 1);
          return this;
        }
      }
    } else {
      this._all = [];
    }
    return this;
  };

  EventEmitter.prototype.removeListener = EventEmitter.prototype.off;

  EventEmitter.prototype.removeAllListeners = function(type) {
    if (arguments.length === 0) {
      !this._events || init.call(this);
      return this;
    }

    if(this.wildcard) {
      var ns = typeof type === 'string' ? type.split(this.delimiter) : type.slice();
      var leafs = searchListenerTree.call(this, null, ns, this.listenerTree, 0);

      for (var iLeaf=0; iLeaf<leafs.length; iLeaf++) {
        var leaf = leafs[iLeaf];
        leaf._listeners = null;
      }
    }
    else {
      if (!this._events[type]) return this;
      this._events[type] = null;
    }
    return this;
  };

  EventEmitter.prototype.listeners = function(type) {
    if(this.wildcard) {
      var handlers = [];
      var ns = typeof type === 'string' ? type.split(this.delimiter) : type.slice();
      searchListenerTree.call(this, handlers, ns, this.listenerTree, 0);
      return handlers;
    }

    this._events || init.call(this);

    if (!this._events[type]) this._events[type] = [];
    if (!isArray(this._events[type])) {
      this._events[type] = [this._events[type]];
    }
    return this._events[type];
  };

  EventEmitter.prototype.listenersAny = function() {

    if(this._all) {
      return this._all;
    }
    else {
      return [];
    }

  };

  if (typeof define === 'function' && define.amd) {
     // AMD. Register as an anonymous module.
    define(function() {
      return EventEmitter;
    });
  } else if (typeof exports === 'object') {
    // CommonJS
    exports.EventEmitter2 = EventEmitter;
  }
  else {
    // Browser global.
    window.EventEmitter2 = EventEmitter;
  }
}();

},{}],3:[function(_dereq_,module,exports){
var diff = _dereq_('fast-diff');
var is = _dereq_('./is');
var op = _dereq_('./op');


var NULL_CHARACTER = String.fromCharCode(0);  // Placeholder char for embed in diff()


var Delta = function (ops) {
  // Assume we are given a well formed ops
  if (is.array(ops)) {
    this.ops = ops;
  } else if (is.object(ops) && is.array(ops.ops)) {
    this.ops = ops.ops;
  } else {
    this.ops = [];
  }
};


Delta.prototype.insert = function (text, attributes) {
  var newOp = {};
  if (is.string(text)) {
    if (text.length === 0) return this;
    newOp.insert = text;
  } else if (is.number(text)) {
    newOp.insert = text;
  }
  if (is.object(attributes) && Object.keys(attributes).length > 0) newOp.attributes = attributes;
  return this.push(newOp);
};

Delta.prototype['delete'] = function (length) {
  if (length <= 0) return this;
  return this.push({ 'delete': length });
};

Delta.prototype.retain = function (length, attributes) {
  if (length <= 0) return this;
  var newOp = { retain: length };
  if (is.object(attributes) && Object.keys(attributes).length > 0) newOp.attributes = attributes;
  return this.push(newOp);
};

Delta.prototype.push = function (newOp) {
  var index = this.ops.length;
  var lastOp = this.ops[index - 1];
  newOp = op.clone(newOp);
  if (is.object(lastOp)) {
    if (is.number(newOp['delete']) && is.number(lastOp['delete'])) {
      this.ops[index - 1] = { 'delete': lastOp['delete'] + newOp['delete'] };
      return this;
    }
    // Since it does not matter if we insert before or after deleting at the same index,
    // always prefer to insert first
    if (is.number(lastOp['delete']) && (is.string(newOp.insert) || is.number(newOp.insert))) {
      index -= 1;
      lastOp = this.ops[index - 1];
      if (!is.object(lastOp)) {
        this.ops.unshift(newOp);
        return this;
      }
    }
    if (is.equal(newOp.attributes, lastOp.attributes)) {
      if (is.string(newOp.insert) && is.string(lastOp.insert)) {
        this.ops[index - 1] = { insert: lastOp.insert + newOp.insert };
        if (is.object(newOp.attributes)) this.ops[index - 1].attributes = newOp.attributes
        return this;
      } else if (is.number(newOp.retain) && is.number(lastOp.retain)) {
        this.ops[index - 1] = { retain: lastOp.retain + newOp.retain };
        if (is.object(newOp.attributes)) this.ops[index - 1].attributes = newOp.attributes
        return this;
      }
    }
  }
  this.ops.splice(index, 0, newOp);
  return this;
};

Delta.prototype.chop = function () {
  var lastOp = this.ops[this.ops.length - 1];
  if (lastOp && lastOp.retain && !lastOp.attributes) {
    this.ops.pop();
  }
  return this;
};

Delta.prototype.length = function () {
  return this.ops.reduce(function (length, elem) {
    return length + op.length(elem);
  }, 0);
};

Delta.prototype.slice = function (start, end) {
  start = start || 0;
  if (!is.number(end)) end = Infinity;
  var delta = new Delta();
  var iter = op.iterator(this.ops);
  var index = 0;
  while (index < end && iter.hasNext()) {
    var nextOp;
    if (index < start) {
      nextOp = iter.next(start - index);
    } else {
      nextOp = iter.next(end - index);
      delta.push(nextOp);
    }
    index += op.length(nextOp);
  }
  return delta;
};


Delta.prototype.compose = function (other) {
  var thisIter = op.iterator(this.ops);
  var otherIter = op.iterator(other.ops);
  this.ops = [];
  while (thisIter.hasNext() || otherIter.hasNext()) {
    if (otherIter.peekType() === 'insert') {
      this.push(otherIter.next());
    } else if (thisIter.peekType() === 'delete') {
      this.push(thisIter.next());
    } else {
      var length = Math.min(thisIter.peekLength(), otherIter.peekLength());
      var thisOp = thisIter.next(length);
      var otherOp = otherIter.next(length);
      if (is.number(otherOp.retain)) {
        var newOp = {};
        if (is.number(thisOp.retain)) {
          newOp.retain = length;
        } else {
          newOp.insert = thisOp.insert;
        }
        // Preserve null when composing with a retain, otherwise remove it for inserts
        var attributes = op.attributes.compose(thisOp.attributes, otherOp.attributes, is.number(thisOp.retain));
        if (attributes) newOp.attributes = attributes;
        this.push(newOp);
      // Other op should be delete, we could be an insert or retain
      // Insert + delete cancels out
      } else if (is.number(otherOp['delete']) && is.number(thisOp.retain)) {
        this.push(otherOp);
      }
    }
  }
  return this.chop();
};

Delta.prototype.diff = function (other) {
  var strings = [this.ops, other.ops].map(function (ops) {
    return ops.map(function (op) {
      if (is.string(op.insert)) return op.insert;
      if (is.number(op.insert)) return NULL_CHARACTER;
      var prep = ops === other.ops ? 'on' : 'with';
      throw new Error('diff() called ' + prep + ' non-document');
    }).join('');
  });
  var diffResult = diff(strings[0], strings[1]);
  var thisIter = op.iterator(this.ops);
  var otherIter = op.iterator(other.ops);
  var delta = new Delta();
  diffResult.forEach(function (component) {
    var length = component[1].length;
    while (length > 0) {
      var opLength = 0;
      switch (component[0]) {
        case diff.INSERT:
          opLength = Math.min(otherIter.peekLength(), length);
          delta.push(otherIter.next(opLength));
          break;
        case diff.DELETE:
          opLength = Math.min(length, thisIter.peekLength());
          thisIter.next(opLength);
          delta['delete'](opLength);
          break;
        case diff.EQUAL:
          opLength = Math.min(thisIter.peekLength(), otherIter.peekLength(), length);
          var thisOp = thisIter.next(opLength);
          var otherOp = otherIter.next(opLength);
          if (thisOp.insert === otherOp.insert) {
            delta.retain(opLength, op.attributes.diff(thisOp.attributes, otherOp.attributes));
          } else {
            delta.push(otherOp)['delete'](opLength);
          }
          break;
      }
      length -= opLength;
    }
  });
  return delta.chop();
};

Delta.prototype.transform = function (other, priority) {
  priority = !!priority;
  if (is.number(other)) {
    return this.transformPosition(other, priority);
  }
  var thisIter = op.iterator(this.ops);
  var otherIter = op.iterator(other.ops);
  var delta = new Delta();
  while (thisIter.hasNext() || otherIter.hasNext()) {
    if (thisIter.peekType() === 'insert' && (priority || otherIter.peekType() !== 'insert')) {
      delta.retain(op.length(thisIter.next()));
    } else if (otherIter.peekType() === 'insert') {
      delta.push(otherIter.next());
    } else {
      var length = Math.min(thisIter.peekLength(), otherIter.peekLength());
      var thisOp = thisIter.next(length);
      var otherOp = otherIter.next(length);
      if (thisOp['delete']) {
        // Our delete either makes their delete redundant or removes their retain
        continue;
      } else if (otherOp['delete']) {
        delta.push(otherOp);
      } else {
        // We retain either their retain or insert
        delta.retain(length, op.attributes.transform(thisOp.attributes, otherOp.attributes, priority));
      }
    }
  }
  return delta.chop();
};

Delta.prototype.transformPosition = function (index, priority) {
  priority = !!priority;
  var thisIter = op.iterator(this.ops);
  var offset = 0;
  while (thisIter.hasNext() && offset <= index) {
    var length = thisIter.peekLength();
    var nextType = thisIter.peekType();
    thisIter.next();
    if (nextType === 'delete') {
      index -= Math.min(length, index - offset);
      continue;
    } else if (nextType === 'insert' && (offset < index || !priority)) {
      index += length;
    }
    offset += length;
  }
  return index;
};


module.exports = Delta;

},{"./is":4,"./op":5,"fast-diff":6}],4:[function(_dereq_,module,exports){
module.exports = {
  equal: function (a, b) {
    if (a === b) return true;
    if (a == null && b == null) return true;
    if (a == null || b == null) return false;
    if (Object.keys(a).length != Object.keys(b).length) return false;
    for(var key in a) {
      // Only compare one level deep
      if (a[key] !== b[key]) return false;
    }
    return true;
  },

  array: function (value) {
    return Array.isArray(value);
  },

  number: function (value) {
    if (typeof value === 'number') return true;
    if (typeof value === 'object' && Object.prototype.toString.call(value) === '[object Number]') return true;
    return false;
  },

  object: function (value) {
    if (!value) return false;
    return (typeof value === 'function' || typeof value === 'object');
  },

  string: function (value) {
    if (typeof value === 'string') return true;
    if (typeof value === 'object' && Object.prototype.toString.call(value) === '[object String]') return true;
    return false;
  }
};

},{}],5:[function(_dereq_,module,exports){
var is = _dereq_('./is');


var lib = {
  attributes: {
    clone: function (attributes, keepNull) {
      if (!is.object(attributes)) return {};
      return Object.keys(attributes).reduce(function (memo, key) {
        if (attributes[key] !== undefined && (attributes[key] !== null || keepNull)) {
          memo[key] = attributes[key];
        }
        return memo;
      }, {});
    },

    compose: function (a, b, keepNull) {
      if (!is.object(a)) a = {};
      if (!is.object(b)) b = {};
      var attributes = this.clone(b, keepNull);
      for (var key in a) {
        if (a[key] !== undefined && b[key] === undefined) {
          attributes[key] = a[key];
        }
      }
      return Object.keys(attributes).length > 0 ? attributes : undefined;
    },

    diff: function(a, b) {
      if (!is.object(a)) a = {};
      if (!is.object(b)) b = {};
      var attributes = Object.keys(a).concat(Object.keys(b)).reduce(function (attributes, key) {
        if (a[key] !== b[key]) {
          attributes[key] = b[key] === undefined ? null : b[key];
        }
        return attributes;
      }, {});
      return Object.keys(attributes).length > 0 ? attributes : undefined;
    },

    transform: function (a, b, priority) {
      if (!is.object(a)) return b;
      if (!is.object(b)) return undefined;
      if (!priority) return b;  // b simply overwrites us without priority
      var attributes = Object.keys(b).reduce(function (attributes, key) {
        if (a[key] === undefined) attributes[key] = b[key];  // null is a valid value
        return attributes;
      }, {});
      return Object.keys(attributes).length > 0 ? attributes : undefined;
    }
  },

  clone: function (op) {
    var newOp = this.attributes.clone(op);
    if (is.object(newOp.attributes)) {
      newOp.attributes = this.attributes.clone(newOp.attributes, true);
    }
    return newOp;
  },

  iterator: function (ops) {
    return new Iterator(ops);
  },

  length: function (op) {
    if (is.number(op['delete'])) {
      return op['delete'];
    } else if (is.number(op.retain)) {
      return op.retain;
    } else {
      return is.string(op.insert) ? op.insert.length : 1;
    }
  }
};


function Iterator(ops) {
  this.ops = ops;
  this.index = 0;
  this.offset = 0;
};

Iterator.prototype.hasNext = function () {
  return this.peekLength() < Infinity;
};

Iterator.prototype.next = function (length) {
  if (!length) length = Infinity;
  var nextOp = this.ops[this.index];
  if (nextOp) {
    var offset = this.offset;
    var opLength = lib.length(nextOp)
    if (length >= opLength - offset) {
      length = opLength - offset;
      this.index += 1;
      this.offset = 0;
    } else {
      this.offset += length;
    }
    if (is.number(nextOp['delete'])) {
      return { 'delete': length };
    } else {
      var retOp = {};
      if (nextOp.attributes) {
        retOp.attributes = nextOp.attributes;
      }
      if (is.number(nextOp.retain)) {
        retOp.retain = length;
      } else if (is.string(nextOp.insert)) {
        retOp.insert = nextOp.insert.substr(offset, length);
      } else {
        // offset should === 0, length should === 1
        retOp.insert = nextOp.insert;
      }
      return retOp;
    }
  } else {
    return { retain: Infinity };
  }
};

Iterator.prototype.peekLength = function () {
  if (this.ops[this.index]) {
    // Should never return 0 if our index is being managed correctly
    return lib.length(this.ops[this.index]) - this.offset;
  } else {
    return Infinity;
  }
};

Iterator.prototype.peekType = function () {
  if (this.ops[this.index]) {
    if (is.number(this.ops[this.index]['delete'])) {
      return 'delete';
    } else if (is.number(this.ops[this.index].retain)) {
      return 'retain';
    } else {
      return 'insert';
    }
  }
  return 'retain';
};


module.exports = lib;

},{"./is":4}],6:[function(_dereq_,module,exports){
/**
 * This library modifies the diff-patch-match library by Neil Fraser
 * by removing the patch and match functionality and certain advanced
 * options in the diff function. The original license is as follows:
 *
 * ===
 *
 * Diff Match and Patch
 *
 * Copyright 2006 Google Inc.
 * http://code.google.com/p/google-diff-match-patch/
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


/**
 * The data structure representing a diff is an array of tuples:
 * [[DIFF_DELETE, 'Hello'], [DIFF_INSERT, 'Goodbye'], [DIFF_EQUAL, ' world.']]
 * which means: delete 'Hello', add 'Goodbye' and keep ' world.'
 */
var DIFF_DELETE = -1;
var DIFF_INSERT = 1;
var DIFF_EQUAL = 0;


/**
 * Find the differences between two texts.  Simplifies the problem by stripping
 * any common prefix or suffix off the texts before diffing.
 * @param {string} text1 Old string to be diffed.
 * @param {string} text2 New string to be diffed.
 * @return {Array} Array of diff tuples.
 */
function diff_main(text1, text2) {
  // Check for equality (speedup).
  if (text1 == text2) {
    if (text1) {
      return [[DIFF_EQUAL, text1]];
    }
    return [];
  }

  // Trim off common prefix (speedup).
  var commonlength = diff_commonPrefix(text1, text2);
  var commonprefix = text1.substring(0, commonlength);
  text1 = text1.substring(commonlength);
  text2 = text2.substring(commonlength);

  // Trim off common suffix (speedup).
  commonlength = diff_commonSuffix(text1, text2);
  var commonsuffix = text1.substring(text1.length - commonlength);
  text1 = text1.substring(0, text1.length - commonlength);
  text2 = text2.substring(0, text2.length - commonlength);

  // Compute the diff on the middle block.
  var diffs = diff_compute_(text1, text2);

  // Restore the prefix and suffix.
  if (commonprefix) {
    diffs.unshift([DIFF_EQUAL, commonprefix]);
  }
  if (commonsuffix) {
    diffs.push([DIFF_EQUAL, commonsuffix]);
  }
  diff_cleanupMerge(diffs);
  return diffs;
};


/**
 * Find the differences between two texts.  Assumes that the texts do not
 * have any common prefix or suffix.
 * @param {string} text1 Old string to be diffed.
 * @param {string} text2 New string to be diffed.
 * @return {Array} Array of diff tuples.
 */
function diff_compute_(text1, text2) {
  var diffs;

  if (!text1) {
    // Just add some text (speedup).
    return [[DIFF_INSERT, text2]];
  }

  if (!text2) {
    // Just delete some text (speedup).
    return [[DIFF_DELETE, text1]];
  }

  var longtext = text1.length > text2.length ? text1 : text2;
  var shorttext = text1.length > text2.length ? text2 : text1;
  var i = longtext.indexOf(shorttext);
  if (i != -1) {
    // Shorter text is inside the longer text (speedup).
    diffs = [[DIFF_INSERT, longtext.substring(0, i)],
             [DIFF_EQUAL, shorttext],
             [DIFF_INSERT, longtext.substring(i + shorttext.length)]];
    // Swap insertions for deletions if diff is reversed.
    if (text1.length > text2.length) {
      diffs[0][0] = diffs[2][0] = DIFF_DELETE;
    }
    return diffs;
  }

  if (shorttext.length == 1) {
    // Single character string.
    // After the previous speedup, the character can't be an equality.
    return [[DIFF_DELETE, text1], [DIFF_INSERT, text2]];
  }

  // Check to see if the problem can be split in two.
  var hm = diff_halfMatch_(text1, text2);
  if (hm) {
    // A half-match was found, sort out the return data.
    var text1_a = hm[0];
    var text1_b = hm[1];
    var text2_a = hm[2];
    var text2_b = hm[3];
    var mid_common = hm[4];
    // Send both pairs off for separate processing.
    var diffs_a = diff_main(text1_a, text2_a);
    var diffs_b = diff_main(text1_b, text2_b);
    // Merge the results.
    return diffs_a.concat([[DIFF_EQUAL, mid_common]], diffs_b);
  }

  return diff_bisect_(text1, text2);
};


/**
 * Find the 'middle snake' of a diff, split the problem in two
 * and return the recursively constructed diff.
 * See Myers 1986 paper: An O(ND) Difference Algorithm and Its Variations.
 * @param {string} text1 Old string to be diffed.
 * @param {string} text2 New string to be diffed.
 * @return {Array} Array of diff tuples.
 * @private
 */
function diff_bisect_(text1, text2) {
  // Cache the text lengths to prevent multiple calls.
  var text1_length = text1.length;
  var text2_length = text2.length;
  var max_d = Math.ceil((text1_length + text2_length) / 2);
  var v_offset = max_d;
  var v_length = 2 * max_d;
  var v1 = new Array(v_length);
  var v2 = new Array(v_length);
  // Setting all elements to -1 is faster in Chrome & Firefox than mixing
  // integers and undefined.
  for (var x = 0; x < v_length; x++) {
    v1[x] = -1;
    v2[x] = -1;
  }
  v1[v_offset + 1] = 0;
  v2[v_offset + 1] = 0;
  var delta = text1_length - text2_length;
  // If the total number of characters is odd, then the front path will collide
  // with the reverse path.
  var front = (delta % 2 != 0);
  // Offsets for start and end of k loop.
  // Prevents mapping of space beyond the grid.
  var k1start = 0;
  var k1end = 0;
  var k2start = 0;
  var k2end = 0;
  for (var d = 0; d < max_d; d++) {
    // Walk the front path one step.
    for (var k1 = -d + k1start; k1 <= d - k1end; k1 += 2) {
      var k1_offset = v_offset + k1;
      var x1;
      if (k1 == -d || (k1 != d && v1[k1_offset - 1] < v1[k1_offset + 1])) {
        x1 = v1[k1_offset + 1];
      } else {
        x1 = v1[k1_offset - 1] + 1;
      }
      var y1 = x1 - k1;
      while (x1 < text1_length && y1 < text2_length &&
             text1.charAt(x1) == text2.charAt(y1)) {
        x1++;
        y1++;
      }
      v1[k1_offset] = x1;
      if (x1 > text1_length) {
        // Ran off the right of the graph.
        k1end += 2;
      } else if (y1 > text2_length) {
        // Ran off the bottom of the graph.
        k1start += 2;
      } else if (front) {
        var k2_offset = v_offset + delta - k1;
        if (k2_offset >= 0 && k2_offset < v_length && v2[k2_offset] != -1) {
          // Mirror x2 onto top-left coordinate system.
          var x2 = text1_length - v2[k2_offset];
          if (x1 >= x2) {
            // Overlap detected.
            return diff_bisectSplit_(text1, text2, x1, y1);
          }
        }
      }
    }

    // Walk the reverse path one step.
    for (var k2 = -d + k2start; k2 <= d - k2end; k2 += 2) {
      var k2_offset = v_offset + k2;
      var x2;
      if (k2 == -d || (k2 != d && v2[k2_offset - 1] < v2[k2_offset + 1])) {
        x2 = v2[k2_offset + 1];
      } else {
        x2 = v2[k2_offset - 1] + 1;
      }
      var y2 = x2 - k2;
      while (x2 < text1_length && y2 < text2_length &&
             text1.charAt(text1_length - x2 - 1) ==
             text2.charAt(text2_length - y2 - 1)) {
        x2++;
        y2++;
      }
      v2[k2_offset] = x2;
      if (x2 > text1_length) {
        // Ran off the left of the graph.
        k2end += 2;
      } else if (y2 > text2_length) {
        // Ran off the top of the graph.
        k2start += 2;
      } else if (!front) {
        var k1_offset = v_offset + delta - k2;
        if (k1_offset >= 0 && k1_offset < v_length && v1[k1_offset] != -1) {
          var x1 = v1[k1_offset];
          var y1 = v_offset + x1 - k1_offset;
          // Mirror x2 onto top-left coordinate system.
          x2 = text1_length - x2;
          if (x1 >= x2) {
            // Overlap detected.
            return diff_bisectSplit_(text1, text2, x1, y1);
          }
        }
      }
    }
  }
  // Diff took too long and hit the deadline or
  // number of diffs equals number of characters, no commonality at all.
  return [[DIFF_DELETE, text1], [DIFF_INSERT, text2]];
};


/**
 * Given the location of the 'middle snake', split the diff in two parts
 * and recurse.
 * @param {string} text1 Old string to be diffed.
 * @param {string} text2 New string to be diffed.
 * @param {number} x Index of split point in text1.
 * @param {number} y Index of split point in text2.
 * @return {Array} Array of diff tuples.
 */
function diff_bisectSplit_(text1, text2, x, y) {
  var text1a = text1.substring(0, x);
  var text2a = text2.substring(0, y);
  var text1b = text1.substring(x);
  var text2b = text2.substring(y);

  // Compute both diffs serially.
  var diffs = diff_main(text1a, text2a);
  var diffsb = diff_main(text1b, text2b);

  return diffs.concat(diffsb);
};


/**
 * Determine the common prefix of two strings.
 * @param {string} text1 First string.
 * @param {string} text2 Second string.
 * @return {number} The number of characters common to the start of each
 *     string.
 */
function diff_commonPrefix(text1, text2) {
  // Quick check for common null cases.
  if (!text1 || !text2 || text1.charAt(0) != text2.charAt(0)) {
    return 0;
  }
  // Binary search.
  // Performance analysis: http://neil.fraser.name/news/2007/10/09/
  var pointermin = 0;
  var pointermax = Math.min(text1.length, text2.length);
  var pointermid = pointermax;
  var pointerstart = 0;
  while (pointermin < pointermid) {
    if (text1.substring(pointerstart, pointermid) ==
        text2.substring(pointerstart, pointermid)) {
      pointermin = pointermid;
      pointerstart = pointermin;
    } else {
      pointermax = pointermid;
    }
    pointermid = Math.floor((pointermax - pointermin) / 2 + pointermin);
  }
  return pointermid;
};


/**
 * Determine the common suffix of two strings.
 * @param {string} text1 First string.
 * @param {string} text2 Second string.
 * @return {number} The number of characters common to the end of each string.
 */
function diff_commonSuffix(text1, text2) {
  // Quick check for common null cases.
  if (!text1 || !text2 ||
      text1.charAt(text1.length - 1) != text2.charAt(text2.length - 1)) {
    return 0;
  }
  // Binary search.
  // Performance analysis: http://neil.fraser.name/news/2007/10/09/
  var pointermin = 0;
  var pointermax = Math.min(text1.length, text2.length);
  var pointermid = pointermax;
  var pointerend = 0;
  while (pointermin < pointermid) {
    if (text1.substring(text1.length - pointermid, text1.length - pointerend) ==
        text2.substring(text2.length - pointermid, text2.length - pointerend)) {
      pointermin = pointermid;
      pointerend = pointermin;
    } else {
      pointermax = pointermid;
    }
    pointermid = Math.floor((pointermax - pointermin) / 2 + pointermin);
  }
  return pointermid;
};


/**
 * Do the two texts share a substring which is at least half the length of the
 * longer text?
 * This speedup can produce non-minimal diffs.
 * @param {string} text1 First string.
 * @param {string} text2 Second string.
 * @return {Array.<string>} Five element Array, containing the prefix of
 *     text1, the suffix of text1, the prefix of text2, the suffix of
 *     text2 and the common middle.  Or null if there was no match.
 */
function diff_halfMatch_(text1, text2) {
  var longtext = text1.length > text2.length ? text1 : text2;
  var shorttext = text1.length > text2.length ? text2 : text1;
  if (longtext.length < 4 || shorttext.length * 2 < longtext.length) {
    return null;  // Pointless.
  }

  /**
   * Does a substring of shorttext exist within longtext such that the substring
   * is at least half the length of longtext?
   * Closure, but does not reference any external variables.
   * @param {string} longtext Longer string.
   * @param {string} shorttext Shorter string.
   * @param {number} i Start index of quarter length substring within longtext.
   * @return {Array.<string>} Five element Array, containing the prefix of
   *     longtext, the suffix of longtext, the prefix of shorttext, the suffix
   *     of shorttext and the common middle.  Or null if there was no match.
   * @private
   */
  function diff_halfMatchI_(longtext, shorttext, i) {
    // Start with a 1/4 length substring at position i as a seed.
    var seed = longtext.substring(i, i + Math.floor(longtext.length / 4));
    var j = -1;
    var best_common = '';
    var best_longtext_a, best_longtext_b, best_shorttext_a, best_shorttext_b;
    while ((j = shorttext.indexOf(seed, j + 1)) != -1) {
      var prefixLength = diff_commonPrefix(longtext.substring(i),
                                           shorttext.substring(j));
      var suffixLength = diff_commonSuffix(longtext.substring(0, i),
                                           shorttext.substring(0, j));
      if (best_common.length < suffixLength + prefixLength) {
        best_common = shorttext.substring(j - suffixLength, j) +
            shorttext.substring(j, j + prefixLength);
        best_longtext_a = longtext.substring(0, i - suffixLength);
        best_longtext_b = longtext.substring(i + prefixLength);
        best_shorttext_a = shorttext.substring(0, j - suffixLength);
        best_shorttext_b = shorttext.substring(j + prefixLength);
      }
    }
    if (best_common.length * 2 >= longtext.length) {
      return [best_longtext_a, best_longtext_b,
              best_shorttext_a, best_shorttext_b, best_common];
    } else {
      return null;
    }
  }

  // First check if the second quarter is the seed for a half-match.
  var hm1 = diff_halfMatchI_(longtext, shorttext,
                             Math.ceil(longtext.length / 4));
  // Check again based on the third quarter.
  var hm2 = diff_halfMatchI_(longtext, shorttext,
                             Math.ceil(longtext.length / 2));
  var hm;
  if (!hm1 && !hm2) {
    return null;
  } else if (!hm2) {
    hm = hm1;
  } else if (!hm1) {
    hm = hm2;
  } else {
    // Both matched.  Select the longest.
    hm = hm1[4].length > hm2[4].length ? hm1 : hm2;
  }

  // A half-match was found, sort out the return data.
  var text1_a, text1_b, text2_a, text2_b;
  if (text1.length > text2.length) {
    text1_a = hm[0];
    text1_b = hm[1];
    text2_a = hm[2];
    text2_b = hm[3];
  } else {
    text2_a = hm[0];
    text2_b = hm[1];
    text1_a = hm[2];
    text1_b = hm[3];
  }
  var mid_common = hm[4];
  return [text1_a, text1_b, text2_a, text2_b, mid_common];
};


/**
 * Reorder and merge like edit sections.  Merge equalities.
 * Any edit section can move as long as it doesn't cross an equality.
 * @param {Array} diffs Array of diff tuples.
 */
function diff_cleanupMerge(diffs) {
  diffs.push([DIFF_EQUAL, '']);  // Add a dummy entry at the end.
  var pointer = 0;
  var count_delete = 0;
  var count_insert = 0;
  var text_delete = '';
  var text_insert = '';
  var commonlength;
  while (pointer < diffs.length) {
    switch (diffs[pointer][0]) {
      case DIFF_INSERT:
        count_insert++;
        text_insert += diffs[pointer][1];
        pointer++;
        break;
      case DIFF_DELETE:
        count_delete++;
        text_delete += diffs[pointer][1];
        pointer++;
        break;
      case DIFF_EQUAL:
        // Upon reaching an equality, check for prior redundancies.
        if (count_delete + count_insert > 1) {
          if (count_delete !== 0 && count_insert !== 0) {
            // Factor out any common prefixies.
            commonlength = diff_commonPrefix(text_insert, text_delete);
            if (commonlength !== 0) {
              if ((pointer - count_delete - count_insert) > 0 &&
                  diffs[pointer - count_delete - count_insert - 1][0] ==
                  DIFF_EQUAL) {
                diffs[pointer - count_delete - count_insert - 1][1] +=
                    text_insert.substring(0, commonlength);
              } else {
                diffs.splice(0, 0, [DIFF_EQUAL,
                                    text_insert.substring(0, commonlength)]);
                pointer++;
              }
              text_insert = text_insert.substring(commonlength);
              text_delete = text_delete.substring(commonlength);
            }
            // Factor out any common suffixies.
            commonlength = diff_commonSuffix(text_insert, text_delete);
            if (commonlength !== 0) {
              diffs[pointer][1] = text_insert.substring(text_insert.length -
                  commonlength) + diffs[pointer][1];
              text_insert = text_insert.substring(0, text_insert.length -
                  commonlength);
              text_delete = text_delete.substring(0, text_delete.length -
                  commonlength);
            }
          }
          // Delete the offending records and add the merged ones.
          if (count_delete === 0) {
            diffs.splice(pointer - count_insert,
                count_delete + count_insert, [DIFF_INSERT, text_insert]);
          } else if (count_insert === 0) {
            diffs.splice(pointer - count_delete,
                count_delete + count_insert, [DIFF_DELETE, text_delete]);
          } else {
            diffs.splice(pointer - count_delete - count_insert,
                count_delete + count_insert, [DIFF_DELETE, text_delete],
                [DIFF_INSERT, text_insert]);
          }
          pointer = pointer - count_delete - count_insert +
                    (count_delete ? 1 : 0) + (count_insert ? 1 : 0) + 1;
        } else if (pointer !== 0 && diffs[pointer - 1][0] == DIFF_EQUAL) {
          // Merge this equality with the previous one.
          diffs[pointer - 1][1] += diffs[pointer][1];
          diffs.splice(pointer, 1);
        } else {
          pointer++;
        }
        count_insert = 0;
        count_delete = 0;
        text_delete = '';
        text_insert = '';
        break;
    }
  }
  if (diffs[diffs.length - 1][1] === '') {
    diffs.pop();  // Remove the dummy entry at the end.
  }

  // Second pass: look for single edits surrounded on both sides by equalities
  // which can be shifted sideways to eliminate an equality.
  // e.g: A<ins>BA</ins>C -> <ins>AB</ins>AC
  var changes = false;
  pointer = 1;
  // Intentionally ignore the first and last element (don't need checking).
  while (pointer < diffs.length - 1) {
    if (diffs[pointer - 1][0] == DIFF_EQUAL &&
        diffs[pointer + 1][0] == DIFF_EQUAL) {
      // This is a single edit surrounded by equalities.
      if (diffs[pointer][1].substring(diffs[pointer][1].length -
          diffs[pointer - 1][1].length) == diffs[pointer - 1][1]) {
        // Shift the edit over the previous equality.
        diffs[pointer][1] = diffs[pointer - 1][1] +
            diffs[pointer][1].substring(0, diffs[pointer][1].length -
                                        diffs[pointer - 1][1].length);
        diffs[pointer + 1][1] = diffs[pointer - 1][1] + diffs[pointer + 1][1];
        diffs.splice(pointer - 1, 1);
        changes = true;
      } else if (diffs[pointer][1].substring(0, diffs[pointer + 1][1].length) ==
          diffs[pointer + 1][1]) {
        // Shift the edit over the next equality.
        diffs[pointer - 1][1] += diffs[pointer + 1][1];
        diffs[pointer][1] =
            diffs[pointer][1].substring(diffs[pointer + 1][1].length) +
            diffs[pointer + 1][1];
        diffs.splice(pointer + 1, 1);
        changes = true;
      }
    }
    pointer++;
  }
  // If shifts were made, the diff needs reordering and another shift sweep.
  if (changes) {
    diff_cleanupMerge(diffs);
  }
};


var diff = diff_main;
diff.INSERT = DIFF_INSERT;
diff.DELETE = DIFF_DELETE;
diff.EQUAL = DIFF_EQUAL;


module.exports = diff;

},{}],7:[function(_dereq_,module,exports){
module.exports={"version":"0.19.7"}
},{}],8:[function(_dereq_,module,exports){
var Delta, Document, Format, Line, LinkedList, Normalizer, dom, _;

_ = _dereq_('lodash');

Delta = _dereq_('rich-text/lib/delta');

dom = _dereq_('../lib/dom');

Format = _dereq_('./format');

Line = _dereq_('./line');

LinkedList = _dereq_('../lib/linked-list');

Normalizer = _dereq_('../lib/normalizer');

Document = (function() {
  function Document(root, options) {
    this.root = root;
    if (options == null) {
      options = {};
    }
    this.formats = {};
    _.each(options.formats, _.bind(this.addFormat, this));
    this.setHTML(this.root.innerHTML);
  }

  Document.prototype.addFormat = function(name, config) {
    if (!_.isObject(config)) {
      config = Format.FORMATS[name];
    }
    if (this.formats[name] != null) {
      console.warn('Overwriting format', name, this.formats[name]);
    }
    return this.formats[name] = new Format(config);
  };

  Document.prototype.appendLine = function(lineNode) {
    return this.insertLineBefore(lineNode, null);
  };

  Document.prototype.findLeafAt = function(index, inclusive) {
    var line, offset, _ref;
    _ref = this.findLineAt(index), line = _ref[0], offset = _ref[1];
    if (line != null) {
      return line.findLeafAt(offset, inclusive);
    } else {
      return [null, offset];
    }
  };

  Document.prototype.findLine = function(node) {
    var line;
    while ((node != null) && (dom.BLOCK_TAGS[node.tagName] == null)) {
      node = node.parentNode;
    }
    line = node != null ? this.lineMap[node.id] : null;
    if ((line != null ? line.node : void 0) === node) {
      return line;
    } else {
      return null;
    }
  };

  Document.prototype.findLineAt = function(index) {
    var curLine, length;
    if (!(this.lines.length > 0)) {
      return [null, index];
    }
    length = this.toDelta().length();
    if (index === length) {
      return [this.lines.last, this.lines.last.length];
    }
    if (index > length) {
      return [null, index - length];
    }
    curLine = this.lines.first;
    while (curLine != null) {
      if (index < curLine.length) {
        return [curLine, index];
      }
      index -= curLine.length;
      curLine = curLine.next;
    }
    return [null, index];
  };

  Document.prototype.getHTML = function() {
    var container, html;
    html = this.root.innerHTML;
    html = html.replace(/\>\s+\</g, '>&nbsp;<');
    container = document.createElement('div');
    container.innerHTML = html;
    _.each(container.querySelectorAll("." + Line.CLASS_NAME), function(node) {
      dom(node).removeClass(Line.CLASS_NAME);
      return node.removeAttribute('id');
    });
    return container.innerHTML;
  };

  Document.prototype.insertLineBefore = function(newLineNode, refLine) {
    var line;
    line = new Line(this, newLineNode);
    if (refLine != null) {
      if (!dom(newLineNode.parentNode).isElement()) {
        this.root.insertBefore(newLineNode, refLine.node);
      }
      this.lines.insertAfter(refLine.prev, line);
    } else {
      if (!dom(newLineNode.parentNode).isElement()) {
        this.root.appendChild(newLineNode);
      }
      this.lines.append(line);
    }
    this.lineMap[line.id] = line;
    return line;
  };

  Document.prototype.mergeLines = function(line, lineToMerge) {
    if (lineToMerge.length > 1) {
      if (line.length === 1) {
        dom(line.leaves.last.node).remove();
      }
      _.each(dom(lineToMerge.node).childNodes(), function(child) {
        if (child.tagName !== dom.DEFAULT_BREAK_TAG) {
          return line.node.appendChild(child);
        }
      });
    }
    this.removeLine(lineToMerge);
    return line.rebuild();
  };

  Document.prototype.optimizeLines = function() {
    return _.each(this.lines.toArray(), function(line, i) {
      line.optimize();
      return true;
    });
  };

  Document.prototype.rebuild = function() {
    var lineNode, lines, _results;
    lines = this.lines.toArray();
    lineNode = this.root.firstChild;
    if ((lineNode != null) && (dom.LIST_TAGS[lineNode.tagName] != null)) {
      lineNode = lineNode.firstChild;
    }
    _.each(lines, (function(_this) {
      return function(line, index) {
        var newLine, _ref;
        while (line.node !== lineNode) {
          if (line.node.parentNode === _this.root || ((_ref = line.node.parentNode) != null ? _ref.parentNode : void 0) === _this.root) {
            lineNode = Normalizer.normalizeLine(lineNode);
            newLine = _this.insertLineBefore(lineNode, line);
            lineNode = dom(lineNode).nextLineNode(_this.root);
          } else {
            return _this.removeLine(line);
          }
        }
        if (line.outerHTML !== lineNode.outerHTML) {
          line.node = Normalizer.normalizeLine(line.node);
          line.rebuild();
        }
        return lineNode = dom(lineNode).nextLineNode(_this.root);
      };
    })(this));
    _results = [];
    while (lineNode != null) {
      lineNode = Normalizer.normalizeLine(lineNode);
      this.appendLine(lineNode);
      _results.push(lineNode = dom(lineNode).nextLineNode(this.root));
    }
    return _results;
  };

  Document.prototype.removeLine = function(line) {
    if (line.node.parentNode != null) {
      if (dom.LIST_TAGS[line.node.parentNode.tagName] && line.node.parentNode.childNodes.length === 1) {
        dom(line.node.parentNode).remove();
      } else {
        dom(line.node).remove();
      }
    }
    delete this.lineMap[line.id];
    return this.lines.remove(line);
  };

  Document.prototype.setHTML = function(html) {
    html = Normalizer.stripComments(html);
    html = Normalizer.stripWhitespace(html);
    this.root.innerHTML = html;
    this.lines = new LinkedList();
    this.lineMap = {};
    return this.rebuild();
  };

  Document.prototype.splitLine = function(line, offset) {
    var lineNode1, lineNode2, newLine, _ref;
    offset = Math.min(offset, line.length - 1);
    _ref = dom(line.node).split(offset, true), lineNode1 = _ref[0], lineNode2 = _ref[1];
    line.node = lineNode1;
    line.rebuild();
    newLine = this.insertLineBefore(lineNode2, line.next);
    newLine.formats = _.clone(line.formats);
    newLine.resetContent();
    return newLine;
  };

  Document.prototype.toDelta = function() {
    var delta, lines;
    lines = this.lines.toArray();
    delta = new Delta();
    _.each(lines, function(line) {
      return _.each(line.delta.ops, function(op) {
        return delta.push(op);
      });
    });
    return delta;
  };

  return Document;

})();

module.exports = Document;



},{"../lib/dom":16,"../lib/linked-list":17,"../lib/normalizer":18,"./format":10,"./line":12,"lodash":1,"rich-text/lib/delta":3}],9:[function(_dereq_,module,exports){
var Document, Editor, Line, Selection, dom, _;

_ = _dereq_('lodash');

dom = _dereq_('../lib/dom');

Document = _dereq_('./document');

Line = _dereq_('./line');

Selection = _dereq_('./selection');

Editor = (function() {
  Editor.sources = {
    API: 'api',
    SILENT: 'silent',
    USER: 'user'
  };

  function Editor(root, quill, options) {
    this.root = root;
    this.quill = quill;
    this.options = options != null ? options : {};
    this.root.setAttribute('id', this.options.id);
    this.doc = new Document(this.root, this.options);
    this.delta = this.doc.toDelta();
    this.selection = new Selection(this.doc, this.quill);
    this.timer = setInterval(_.bind(this.checkUpdate, this), this.options.pollInterval);
    if (!this.options.readOnly) {
      this.enable();
    }
  }

  Editor.prototype.destroy = function() {
    return clearInterval(this.timer);
  };

  Editor.prototype.disable = function() {
    return this.enable(false);
  };

  Editor.prototype.enable = function(enabled) {
    if (enabled == null) {
      enabled = true;
    }
    return this.root.setAttribute('contenteditable', enabled);
  };

  Editor.prototype.applyDelta = function(delta, source) {
    var localDelta;
    localDelta = this._update();
    if (localDelta) {
      delta = localDelta.transform(delta, true);
      localDelta = delta.transform(localDelta, false);
    }
    if (delta.ops.length > 0) {
      delta = this._trackDelta((function(_this) {
        return function() {
          var index;
          index = 0;
          _.each(delta.ops, function(op) {
            if (_.isString(op.insert)) {
              _this._insertAt(index, op.insert, op.attributes);
              return index += op.insert.length;
            } else if (_.isNumber(op.insert)) {
              _this._insertAt(index, dom.EMBED_TEXT, op.attributes);
              return index += 1;
            } else if (_.isNumber(op["delete"])) {
              return _this._deleteAt(index, op["delete"]);
            } else if (_.isNumber(op.retain)) {
              _.each(op.attributes, function(value, name) {
                return _this._formatAt(index, op.retain, name, value);
              });
              return index += op.retain;
            }
          });
          return _this.selection.shiftAfter(0, 0, _.bind(_this.doc.optimizeLines, _this.doc));
        };
      })(this));
      this.delta = this.doc.toDelta();
      this.innerHTML = this.root.innerHTML;
      if (delta && source !== Editor.sources.SILENT) {
        this.quill.emit(this.quill.constructor.events.TEXT_CHANGE, delta, source);
      }
    }
    if (localDelta && localDelta.ops.length > 0 && source !== Editor.sources.SILENT) {
      return this.quill.emit(this.quill.constructor.events.TEXT_CHANGE, localDelta, Editor.sources.USER);
    }
  };

  Editor.prototype.checkUpdate = function(source) {
    var delta;
    if (source == null) {
      source = 'user';
    }
    if (this.root.parentNode == null) {
      return clearInterval(this.timer);
    }
    delta = this._update();
    if (delta) {
      this.delta.compose(delta);
      this.quill.emit(this.quill.constructor.events.TEXT_CHANGE, delta, source);
    }
    if (delta) {
      source = Editor.sources.SILENT;
    }
    return this.selection.update(source);
  };

  Editor.prototype.focus = function() {
    if (this.selection.range != null) {
      return this.selection.setRange(this.selection.range);
    } else {
      return this.root.focus();
    }
  };

  Editor.prototype.getBounds = function(index) {
    var bounds, containerBounds, leaf, offset, range, side, _ref;
    this.checkUpdate();
    _ref = this.doc.findLeafAt(index, true), leaf = _ref[0], offset = _ref[1];
    if (leaf == null) {
      throw new Error('Invalid index');
    }
    containerBounds = this.root.parentNode.getBoundingClientRect();
    side = 'left';
    if (leaf.length === 0) {
      bounds = leaf.node.parentNode.getBoundingClientRect();
    } else {
      range = document.createRange();
      if (offset < leaf.length) {
        range.setStart(leaf.node, offset);
        range.setEnd(leaf.node, offset + 1);
      } else {
        range.setStart(leaf.node, offset - 1);
        range.setEnd(leaf.node, offset);
        side = 'right';
      }
      bounds = range.getBoundingClientRect();
    }
    return {
      height: bounds.height,
      left: bounds[side] - containerBounds.left,
      top: bounds.top - containerBounds.top
    };
  };

  Editor.prototype.getDelta = function() {
    return this.delta;
  };

  Editor.prototype._deleteAt = function(index, length) {
    if (length <= 0) {
      return;
    }
    return this.selection.shiftAfter(index, -1 * length, (function(_this) {
      return function() {
        var curLine, deleteLength, firstLine, mergeFirstLine, nextLine, offset, _ref;
        _ref = _this.doc.findLineAt(index), firstLine = _ref[0], offset = _ref[1];
        curLine = firstLine;
        mergeFirstLine = firstLine.length - offset <= length && offset > 0;
        while ((curLine != null) && length > 0) {
          nextLine = curLine.next;
          deleteLength = Math.min(curLine.length - offset, length);
          if (offset === 0 && length >= curLine.length) {
            _this.doc.removeLine(curLine);
          } else {
            curLine.deleteText(offset, deleteLength);
          }
          length -= deleteLength;
          curLine = nextLine;
          offset = 0;
        }
        if (mergeFirstLine && firstLine.next) {
          return _this.doc.mergeLines(firstLine, firstLine.next);
        }
      };
    })(this));
  };

  Editor.prototype._formatAt = function(index, length, name, value) {
    return this.selection.shiftAfter(index, 0, (function(_this) {
      return function() {
        var formatLength, line, offset, _ref, _results;
        _ref = _this.doc.findLineAt(index), line = _ref[0], offset = _ref[1];
        _results = [];
        while ((line != null) && length > 0) {
          formatLength = Math.min(length, line.length - offset - 1);
          line.formatText(offset, formatLength, name, value);
          length -= formatLength;
          if (length > 0) {
            line.format(name, value);
          }
          length -= 1;
          offset = 0;
          _results.push(line = line.next);
        }
        return _results;
      };
    })(this));
  };

  Editor.prototype._insertAt = function(index, text, formatting) {
    if (formatting == null) {
      formatting = {};
    }
    return this.selection.shiftAfter(index, text.length, (function(_this) {
      return function() {
        var line, lineTexts, offset, _ref;
        text = text.replace(/\r\n?/g, '\n');
        lineTexts = text.split('\n');
        _ref = _this.doc.findLineAt(index), line = _ref[0], offset = _ref[1];
        return _.each(lineTexts, function(lineText, i) {
          var nextLine;
          if ((line == null) || line.length <= offset) {
            if (i < lineTexts.length - 1 || lineText.length > 0) {
              line = _this.doc.appendLine(document.createElement(dom.DEFAULT_BLOCK_TAG));
              offset = 0;
              line.insertText(offset, lineText, formatting);
              line.format(formatting);
              nextLine = null;
            }
          } else {
            line.insertText(offset, lineText, formatting);
            if (i < lineTexts.length - 1) {
              nextLine = _this.doc.splitLine(line, offset + lineText.length);
              _.each(_.defaults({}, formatting, line.formats), function(value, format) {
                return line.format(format, formatting[format]);
              });
              offset = 0;
            }
          }
          return line = nextLine;
        });
      };
    })(this));
  };

  Editor.prototype._trackDelta = function(fn) {
    var delta, newDelta;
    fn();
    newDelta = this.doc.toDelta();
    delta = this.delta.diff(newDelta);
    return delta;
  };

  Editor.prototype._update = function() {
    var delta;
    if (this.innerHTML === this.root.innerHTML) {
      return false;
    }
    delta = this._trackDelta((function(_this) {
      return function() {
        _this.selection.preserve(_.bind(_this.doc.rebuild, _this.doc));
        return _this.selection.shiftAfter(0, 0, _.bind(_this.doc.optimizeLines, _this.doc));
      };
    })(this));
    this.innerHTML = this.root.innerHTML;
    if (delta.ops.length > 0) {
      return delta;
    } else {
      return false;
    }
  };

  return Editor;

})();

module.exports = Editor;



},{"../lib/dom":16,"./document":8,"./line":12,"./selection":13,"lodash":1}],10:[function(_dereq_,module,exports){
var Format, dom, _;

_ = _dereq_('lodash');

dom = _dereq_('../lib/dom');

Format = (function() {
  Format.types = {
    LINE: 'line'
  };

  Format.FORMATS = {
    bold: {
      tag: 'B',
      prepare: 'bold'
    },
    italic: {
      tag: 'I',
      prepare: 'italic'
    },
    underline: {
      tag: 'U',
      prepare: 'underline'
    },
    strike: {
      tag: 'S',
      prepare: 'strikeThrough'
    },
    color: {
      style: 'color',
      "default": 'rgb(0, 0, 0)',
      prepare: 'foreColor'
    },
    background: {
      style: 'backgroundColor',
      "default": 'rgb(255, 255, 255)',
      prepare: 'backColor'
    },
    font: {
      style: 'fontFamily',
      "default": "'Helvetica', 'Arial', sans-serif",
      prepare: 'fontName'
    },
    size: {
      style: 'fontSize',
      "default": '13px',
      prepare: function(value) {
        return document.execCommand('fontSize', false, dom.convertFontSize(value));
      }
    },
    link: {
      tag: 'A',
      attribute: 'href'
    },
    image: {
      tag: 'IMG',
      attribute: 'src'
    },
    align: {
      type: Format.types.LINE,
      style: 'textAlign',
      "default": 'left'
    },
    bullet: {
      type: Format.types.LINE,
      exclude: 'list',
      parentTag: 'UL',
      tag: 'LI'
    },
    list: {
      type: Format.types.LINE,
      exclude: 'bullet',
      parentTag: 'OL',
      tag: 'LI'
    }
  };

  function Format(config) {
    this.config = config;
  }

  Format.prototype.add = function(node, value) {
    var formatNode, inline, parentNode, _ref, _ref1;
    if (!value) {
      return this.remove(node);
    }
    if (this.value(node) === value) {
      return node;
    }
    if (_.isString(this.config.parentTag)) {
      parentNode = document.createElement(this.config.parentTag);
      dom(node).wrap(parentNode);
      if (node.parentNode.tagName === ((_ref = node.parentNode.previousSibling) != null ? _ref.tagName : void 0)) {
        dom(node.parentNode.previousSibling).merge(node.parentNode);
      }
      if (node.parentNode.tagName === ((_ref1 = node.parentNode.nextSibling) != null ? _ref1.tagName : void 0)) {
        dom(node.parentNode).merge(node.parentNode.nextSibling);
      }
    }
    if (_.isString(this.config.tag)) {
      formatNode = document.createElement(this.config.tag);
      if (dom.VOID_TAGS[formatNode.tagName] != null) {
        if (node.parentNode != null) {
          dom(node).replace(formatNode);
        }
        node = formatNode;
      } else if (this.isType(Format.types.LINE)) {
        node = dom(node).switchTag(this.config.tag);
      } else {
        dom(node).wrap(formatNode);
        node = formatNode;
      }
    }
    if (_.isString(this.config.style) || _.isString(this.config.attribute) || _.isString(this.config["class"])) {
      if (_.isString(this.config["class"])) {
        node = this.remove(node);
      }
      if (dom(node).isTextNode()) {
        inline = document.createElement(dom.DEFAULT_INLINE_TAG);
        dom(node).wrap(inline);
        node = inline;
      }
      if (_.isString(this.config.style)) {
        if (value !== this.config["default"]) {
          node.style[this.config.style] = value;
        }
      }
      if (_.isString(this.config.attribute)) {
        node.setAttribute(this.config.attribute, value);
      }
      if (_.isString(this.config["class"])) {
        dom(node).addClass(this.config["class"] + value);
      }
    }
    return node;
  };

  Format.prototype.isType = function(type) {
    return type === this.config.type;
  };

  Format.prototype.match = function(node) {
    var c, _i, _len, _ref, _ref1;
    if (!dom(node).isElement()) {
      return false;
    }
    if (_.isString(this.config.parentTag) && ((_ref = node.parentNode) != null ? _ref.tagName : void 0) !== this.config.parentTag) {
      return false;
    }
    if (_.isString(this.config.tag) && node.tagName !== this.config.tag) {
      return false;
    }
    if (_.isString(this.config.style) && (!node.style[this.config.style] || node.style[this.config.style] === this.config["default"])) {
      return false;
    }
    if (_.isString(this.config.attribute) && !node.hasAttribute(this.config.attribute)) {
      return false;
    }
    if (_.isString(this.config["class"])) {
      _ref1 = dom(node).classes();
      for (_i = 0, _len = _ref1.length; _i < _len; _i++) {
        c = _ref1[_i];
        if (c.indexOf(this.config["class"]) === 0) {
          return true;
        }
      }
      return false;
    }
    return true;
  };

  Format.prototype.prepare = function(value) {
    if (_.isString(this.config.prepare)) {
      return document.execCommand(this.config.prepare, false, value);
    } else if (_.isFunction(this.config.prepare)) {
      return this.config.prepare(value);
    }
  };

  Format.prototype.remove = function(node) {
    var c, _i, _len, _ref;
    if (!this.match(node)) {
      return node;
    }
    if (_.isString(this.config.style)) {
      node.style[this.config.style] = '';
      if (!node.getAttribute('style')) {
        node.removeAttribute('style');
      }
    }
    if (_.isString(this.config.attribute)) {
      node.removeAttribute(this.config.attribute);
    }
    if (_.isString(this.config["class"])) {
      _ref = dom(node).classes();
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        c = _ref[_i];
        if (c.indexOf(this.config["class"]) === 0) {
          dom(node).removeClass(c);
        }
      }
    }
    if (_.isString(this.config.tag)) {
      if (this.isType(Format.types.LINE)) {
        if (_.isString(this.config.parentTag)) {
          if (node.previousSibling != null) {
            dom(node).splitAncestors(node.parentNode.parentNode);
          }
          if (node.nextSibling != null) {
            dom(node.nextSibling).splitAncestors(node.parentNode.parentNode);
          }
        }
        node = dom(node).switchTag(dom.DEFAULT_BLOCK_TAG);
      } else {
        node = dom(node).switchTag(dom.DEFAULT_INLINE_TAG);
        if (dom.EMBED_TAGS[this.config.tag] != null) {
          dom(node).text(dom.EMBED_TEXT);
        }
      }
    }
    if (_.isString(this.config.parentTag)) {
      dom(node.parentNode).unwrap();
    }
    if (node.tagName === dom.DEFAULT_INLINE_TAG && !node.hasAttributes()) {
      node = dom(node).unwrap();
    }
    return node;
  };

  Format.prototype.value = function(node) {
    var c, _i, _len, _ref;
    if (!this.match(node)) {
      return void 0;
    }
    if (_.isString(this.config.attribute)) {
      return node.getAttribute(this.config.attribute) || void 0;
    } else if (_.isString(this.config.style)) {
      return node.style[this.config.style] || void 0;
    } else if (_.isString(this.config["class"])) {
      _ref = dom(node).classes();
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        c = _ref[_i];
        if (c.indexOf(this.config["class"]) === 0) {
          return c.slice(this.config["class"].length);
        }
      }
    } else if (_.isString(this.config.tag)) {
      return true;
    }
    return void 0;
  };

  return Format;

})();

module.exports = Format;



},{"../lib/dom":16,"lodash":1}],11:[function(_dereq_,module,exports){
var Format, Leaf, LinkedList, dom, _,
  __hasProp = {}.hasOwnProperty,
  __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

_ = _dereq_('lodash');

dom = _dereq_('../lib/dom');

Format = _dereq_('./format');

LinkedList = _dereq_('../lib/linked-list');

Leaf = (function(_super) {
  __extends(Leaf, _super);

  Leaf.ID_PREFIX = 'ql-leaf-';

  Leaf.isLeafNode = function(node) {
    return dom(node).isTextNode() || (node.firstChild == null);
  };

  function Leaf(node, formats) {
    this.node = node;
    this.formats = _.clone(formats);
    this.id = _.uniqueId(Leaf.ID_PREFIX);
    this.text = dom(this.node).text();
    this.length = this.text.length;
  }

  Leaf.prototype.deleteText = function(offset, length) {
    var textNode;
    if (!(length > 0)) {
      return;
    }
    this.text = this.text.slice(0, offset) + this.text.slice(offset + length);
    this.length = this.text.length;
    if (dom.EMBED_TAGS[this.node.tagName] != null) {
      textNode = document.createTextNode(this.text);
      return this.node = dom(this.node).replace(textNode);
    } else {
      return dom(this.node).text(this.text);
    }
  };

  Leaf.prototype.insertText = function(offset, text) {
    var textNode;
    this.text = this.text.slice(0, offset) + text + this.text.slice(offset);
    if (dom(this.node).isTextNode()) {
      dom(this.node).text(this.text);
    } else {
      textNode = document.createTextNode(text);
      if (this.node.tagName === dom.DEFAULT_BREAK_TAG) {
        this.node = dom(this.node).replace(textNode);
      } else {
        this.node.appendChild(textNode);
        this.node = textNode;
      }
    }
    return this.length = this.text.length;
  };

  return Leaf;

})(LinkedList.Node);

module.exports = Leaf;



},{"../lib/dom":16,"../lib/linked-list":17,"./format":10,"lodash":1}],12:[function(_dereq_,module,exports){
var Delta, Format, Leaf, Line, LinkedList, Normalizer, dom, _,
  __hasProp = {}.hasOwnProperty,
  __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

_ = _dereq_('lodash');

Delta = _dereq_('rich-text/lib/delta');

dom = _dereq_('../lib/dom');

Format = _dereq_('./format');

Leaf = _dereq_('./leaf');

LinkedList = _dereq_('../lib/linked-list');

Normalizer = _dereq_('../lib/normalizer');

Line = (function(_super) {
  __extends(Line, _super);

  Line.CLASS_NAME = 'ql-line';

  Line.ID_PREFIX = 'ql-line-';

  function Line(doc, node) {
    this.doc = doc;
    this.node = node;
    this.id = _.uniqueId(Line.ID_PREFIX);
    this.formats = {};
    dom(this.node).addClass(Line.CLASS_NAME);
    this.rebuild();
    Line.__super__.constructor.call(this, this.node);
  }

  Line.prototype.buildLeaves = function(node, formats) {
    return _.each(dom(node).childNodes(), (function(_this) {
      return function(node) {
        var nodeFormats;
        node = Normalizer.normalizeNode(node);
        nodeFormats = _.clone(formats);
        _.each(_this.doc.formats, function(format, name) {
          if (!format.isType(Format.types.LINE) && format.match(node)) {
            return nodeFormats[name] = format.value(node);
          }
        });
        if (Leaf.isLeafNode(node)) {
          return _this.leaves.append(new Leaf(node, nodeFormats));
        } else {
          return _this.buildLeaves(node, nodeFormats);
        }
      };
    })(this));
  };

  Line.prototype.deleteText = function(offset, length) {
    var deleteLength, leaf, _ref;
    if (!(length > 0)) {
      return;
    }
    _ref = this.findLeafAt(offset), leaf = _ref[0], offset = _ref[1];
    while ((leaf != null) && length > 0) {
      deleteLength = Math.min(length, leaf.length - offset);
      leaf.deleteText(offset, deleteLength);
      length -= deleteLength;
      leaf = leaf.next;
      offset = 0;
    }
    return this.rebuild();
  };

  Line.prototype.findLeaf = function(leafNode) {
    var curLeaf;
    curLeaf = this.leaves.first;
    while (curLeaf != null) {
      if (curLeaf.node === leafNode) {
        return curLeaf;
      }
      curLeaf = curLeaf.next;
    }
    return null;
  };

  Line.prototype.findLeafAt = function(offset, inclusive) {
    var leaf;
    if (inclusive == null) {
      inclusive = false;
    }
    if (offset >= this.length - 1) {
      return [this.leaves.last, this.leaves.last.length];
    }
    leaf = this.leaves.first;
    while (leaf != null) {
      if (offset < leaf.length || (offset === leaf.length && inclusive)) {
        return [leaf, offset];
      }
      offset -= leaf.length;
      leaf = leaf.next;
    }
    return [this.leaves.last, offset - this.leaves.last.length];
  };

  Line.prototype.format = function(name, value) {
    var formats;
    if (_.isObject(name)) {
      formats = name;
    } else {
      formats = {};
      formats[name] = value;
    }
    _.each(formats, (function(_this) {
      return function(value, name) {
        var excludeFormat, format;
        format = _this.doc.formats[name];
        if (format == null) {
          return;
        }
        if (format.isType(Format.types.LINE)) {
          if (format.config.exclude && _this.formats[format.config.exclude]) {
            excludeFormat = _this.doc.formats[format.config.exclude];
            if (excludeFormat != null) {
              _this.node = excludeFormat.remove(_this.node);
              delete _this.formats[format.config.exclude];
            }
          }
          _this.node = format.add(_this.node, value);
        }
        if (value) {
          return _this.formats[name] = value;
        } else {
          return delete _this.formats[name];
        }
      };
    })(this));
    return this.resetContent();
  };

  Line.prototype.formatText = function(offset, length, name, value) {
    var format, leaf, leafOffset, leftNode, nextLeaf, rightNode, targetNode, _ref, _ref1, _ref2;
    _ref = this.findLeafAt(offset), leaf = _ref[0], leafOffset = _ref[1];
    format = this.doc.formats[name];
    if (!((format != null) && format.config.type !== Format.types.LINE)) {
      return;
    }
    while ((leaf != null) && length > 0) {
      nextLeaf = leaf.next;
      if ((value && leaf.formats[name] !== value) || (!value && (leaf.formats[name] != null))) {
        targetNode = leaf.node;
        if (leaf.formats[name] != null) {
          dom(targetNode).splitAncestors(this.node);
          while (!format.match(targetNode)) {
            targetNode = targetNode.parentNode;
          }
        }
        if (leafOffset > 0) {
          _ref1 = dom(targetNode).split(leafOffset), leftNode = _ref1[0], targetNode = _ref1[1];
        }
        if (leaf.length > leafOffset + length) {
          _ref2 = dom(targetNode).split(length), targetNode = _ref2[0], rightNode = _ref2[1];
        }
        format.add(targetNode, value);
      }
      length -= leaf.length - leafOffset;
      leafOffset = 0;
      leaf = nextLeaf;
    }
    return this.rebuild();
  };

  Line.prototype.insertText = function(offset, text, formats) {
    var leaf, leafOffset, nextNode, node, prevNode, _ref, _ref1;
    if (formats == null) {
      formats = {};
    }
    if (!(text.length > 0)) {
      return;
    }
    _ref = this.findLeafAt(offset), leaf = _ref[0], leafOffset = _ref[1];
    if (_.isEqual(leaf.formats, formats)) {
      leaf.insertText(leafOffset, text);
      return this.resetContent();
    } else {
      node = _.reduce(formats, (function(_this) {
        return function(node, value, name) {
          var format;
          format = _this.doc.formats[name];
          if (format != null) {
            node = format.add(node, value);
          }
          return node;
        };
      })(this), document.createTextNode(text));
      _ref1 = dom(leaf.node).split(leafOffset), prevNode = _ref1[0], nextNode = _ref1[1];
      if (nextNode) {
        nextNode = dom(nextNode).splitAncestors(this.node).get();
      }
      this.node.insertBefore(node, nextNode);
      return this.rebuild();
    }
  };

  Line.prototype.optimize = function() {
    Normalizer.optimizeLine(this.node);
    return this.rebuild();
  };

  Line.prototype.rebuild = function(force) {
    if (force == null) {
      force = false;
    }
    if (!force && (this.outerHTML != null) && this.outerHTML === this.node.outerHTML) {
      if (_.all(this.leaves.toArray(), (function(_this) {
        return function(leaf) {
          return dom(leaf.node).isAncestor(_this.node);
        };
      })(this))) {
        return false;
      }
    }
    this.node = Normalizer.normalizeNode(this.node);
    if (dom(this.node).length() === 0 && !this.node.querySelector(dom.DEFAULT_BREAK_TAG)) {
      this.node.appendChild(document.createElement(dom.DEFAULT_BREAK_TAG));
    }
    this.leaves = new LinkedList();
    this.formats = _.reduce(this.doc.formats, (function(_this) {
      return function(formats, format, name) {
        if (format.isType(Format.types.LINE)) {
          if (format.match(_this.node)) {
            formats[name] = format.value(_this.node);
          } else {
            delete formats[name];
          }
        }
        return formats;
      };
    })(this), this.formats);
    this.buildLeaves(this.node, {});
    this.resetContent();
    return true;
  };

  Line.prototype.resetContent = function() {
    if (this.node.id !== this.id) {
      this.node.id = this.id;
    }
    this.outerHTML = this.node.outerHTML;
    this.length = 1;
    this.delta = new Delta();
    _.each(this.leaves.toArray(), (function(_this) {
      return function(leaf) {
        _this.length += leaf.length;
        if (dom.EMBED_TAGS[leaf.node.tagName] != null) {
          return _this.delta.insert(1, leaf.formats);
        } else {
          return _this.delta.insert(leaf.text, leaf.formats);
        }
      };
    })(this));
    return this.delta.insert('\n', this.formats);
  };

  return Line;

})(LinkedList.Node);

module.exports = Line;



},{"../lib/dom":16,"../lib/linked-list":17,"../lib/normalizer":18,"./format":10,"./leaf":11,"lodash":1,"rich-text/lib/delta":3}],13:[function(_dereq_,module,exports){
var Leaf, Normalizer, Range, Selection, dom, _;

_ = _dereq_('lodash');

dom = _dereq_('../lib/dom');

Leaf = _dereq_('./leaf');

Normalizer = _dereq_('../lib/normalizer');

Range = _dereq_('../lib/range');

Selection = (function() {
  function Selection(doc, emitter) {
    this.doc = doc;
    this.emitter = emitter;
    this.focus = false;
    this.range = new Range(0, 0);
    this.nullDelay = false;
    this.update('silent');
  }

  Selection.prototype.checkFocus = function() {
    return document.activeElement === this.doc.root;
  };

  Selection.prototype.getRange = function(ignoreFocus) {
    var end, nativeRange, start;
    if (ignoreFocus == null) {
      ignoreFocus = false;
    }
    if (this.checkFocus()) {
      nativeRange = this._getNativeRange();
      if (nativeRange == null) {
        return null;
      }
      start = this._positionToIndex(nativeRange.startContainer, nativeRange.startOffset);
      if (nativeRange.startContainer === nativeRange.endContainer && nativeRange.startOffset === nativeRange.endOffset) {
        end = start;
      } else {
        end = this._positionToIndex(nativeRange.endContainer, nativeRange.endOffset);
      }
      return new Range(Math.min(start, end), Math.max(start, end));
    } else if (ignoreFocus) {
      return this.range;
    } else {
      return null;
    }
  };

  Selection.prototype.preserve = function(fn) {
    var endNode, endOffset, nativeRange, startNode, startOffset, _ref, _ref1, _ref2, _ref3;
    nativeRange = this._getNativeRange();
    if ((nativeRange != null) && this.checkFocus()) {
      _ref = this._encodePosition(nativeRange.startContainer, nativeRange.startOffset), startNode = _ref[0], startOffset = _ref[1];
      _ref1 = this._encodePosition(nativeRange.endContainer, nativeRange.endOffset), endNode = _ref1[0], endOffset = _ref1[1];
      fn();
      _ref2 = this._decodePosition(startNode, startOffset), startNode = _ref2[0], startOffset = _ref2[1];
      _ref3 = this._decodePosition(endNode, endOffset), endNode = _ref3[0], endOffset = _ref3[1];
      return this._setNativeRange(startNode, startOffset, endNode, endOffset);
    } else {
      return fn();
    }
  };

  Selection.prototype.setRange = function(range, source) {
    var endNode, endOffset, startNode, startOffset, _ref, _ref1, _ref2;
    if (range != null) {
      _ref = this._indexToPosition(range.start), startNode = _ref[0], startOffset = _ref[1];
      if (range.isCollapsed()) {
        _ref1 = [startNode, startOffset], endNode = _ref1[0], endOffset = _ref1[1];
      } else {
        _ref2 = this._indexToPosition(range.end), endNode = _ref2[0], endOffset = _ref2[1];
      }
      this._setNativeRange(startNode, startOffset, endNode, endOffset);
    } else {
      this._setNativeRange(null);
    }
    return this.update(source);
  };

  Selection.prototype.shiftAfter = function(index, length, fn) {
    var range;
    range = this.getRange();
    fn();
    if (range != null) {
      range.shift(index, length);
      return this.setRange(range, 'silent');
    }
  };

  Selection.prototype.update = function(source) {
    var emit, focus, range, toEmit;
    focus = this.checkFocus();
    range = this.getRange(true);
    emit = source !== 'silent' && (!Range.compare(range, this.range) || focus !== this.focus);
    toEmit = focus ? range : null;
    if (toEmit === null && source === 'user' && !this.nullDelay) {
      return this.nullDelay = true;
    } else {
      this.nullDelay = false;
      this.range = range;
      this.focus = focus;
      if (emit) {
        return this.emitter.emit(this.emitter.constructor.events.SELECTION_CHANGE, toEmit, source);
      }
    }
  };

  Selection.prototype._decodePosition = function(node, offset) {
    var childIndex;
    if (dom(node).isElement()) {
      childIndex = dom(node.parentNode).childNodes().indexOf(node);
      offset += childIndex;
      node = node.parentNode;
    }
    return [node, offset];
  };

  Selection.prototype._encodePosition = function(node, offset) {
    var text;
    while (true) {
      if (dom(node).isTextNode() || node.tagName === dom.DEFAULT_BREAK_TAG || (dom.EMBED_TAGS[node.tagName] != null)) {
        return [node, offset];
      } else if (offset < node.childNodes.length) {
        node = node.childNodes[offset];
        offset = 0;
      } else if (node.childNodes.length === 0) {
        if (Normalizer.TAGS[node.tagName] == null) {
          text = document.createTextNode('');
          node.appendChild(text);
          node = text;
        }
        return [node, 0];
      } else {
        node = node.lastChild;
        if (dom(node).isElement()) {
          if (node.tagName === dom.DEFAULT_BREAK_TAG || (dom.EMBED_TAGS[node.tagName] != null)) {
            return [node, 1];
          } else {
            offset = node.childNodes.length;
          }
        } else {
          return [node, dom(node).length()];
        }
      }
    }
  };

  Selection.prototype._getNativeRange = function() {
    var range, selection;
    selection = document.getSelection();
    if ((selection != null ? selection.rangeCount : void 0) > 0) {
      range = selection.getRangeAt(0);
      if (dom(range.startContainer).isAncestor(this.doc.root, true)) {
        if (range.startContainer === range.endContainer || dom(range.endContainer).isAncestor(this.doc.root, true)) {
          return range;
        }
      }
    }
    return null;
  };

  Selection.prototype._indexToPosition = function(index) {
    var leaf, offset, _ref;
    if (this.doc.lines.length === 0) {
      return [this.doc.root, 0];
    }
    _ref = this.doc.findLeafAt(index, true), leaf = _ref[0], offset = _ref[1];
    return this._decodePosition(leaf.node, offset);
  };

  Selection.prototype._positionToIndex = function(node, offset) {
    var leaf, leafNode, leafOffset, line, lineOffset, _ref;
    _ref = this._encodePosition(node, offset), leafNode = _ref[0], offset = _ref[1];
    line = this.doc.findLine(leafNode);
    if (line == null) {
      return 0;
    }
    leaf = line.findLeaf(leafNode);
    lineOffset = 0;
    while (line.prev != null) {
      line = line.prev;
      lineOffset += line.length;
    }
    if (leaf == null) {
      return lineOffset;
    }
    leafOffset = 0;
    while (leaf.prev != null) {
      leaf = leaf.prev;
      leafOffset += leaf.length;
    }
    return lineOffset + leafOffset + offset;
  };

  Selection.prototype._setNativeRange = function(startNode, startOffset, endNode, endOffset) {
    var nativeRange, selection;
    selection = document.getSelection();
    if (!selection) {
      return;
    }
    if (startNode != null) {
      if (!this.checkFocus()) {
        this.doc.root.focus();
      }
      nativeRange = this._getNativeRange();
      if ((nativeRange == null) || startNode !== nativeRange.startContainer || startOffset !== nativeRange.startOffset || endNode !== nativeRange.endContainer || endOffset !== nativeRange.endOffset) {
        selection.removeAllRanges();
        nativeRange = document.createRange();
        nativeRange.setStart(startNode, startOffset);
        nativeRange.setEnd(endNode, endOffset);
        return selection.addRange(nativeRange);
      }
    } else {
      selection.removeAllRanges();
      return this.doc.root.blur();
    }
  };

  return Selection;

})();

module.exports = Selection;



},{"../lib/dom":16,"../lib/normalizer":18,"../lib/range":20,"./leaf":11,"lodash":1}],14:[function(_dereq_,module,exports){
_dereq_('./modules/authorship');

_dereq_('./modules/image-tooltip');

_dereq_('./modules/keyboard');

_dereq_('./modules/link-tooltip');

_dereq_('./modules/multi-cursor');

_dereq_('./modules/paste-manager');

_dereq_('./modules/toolbar');

_dereq_('./modules/tooltip');

_dereq_('./modules/undo-manager');

module.exports = _dereq_('./quill');



},{"./modules/authorship":21,"./modules/image-tooltip":22,"./modules/keyboard":23,"./modules/link-tooltip":24,"./modules/multi-cursor":25,"./modules/paste-manager":26,"./modules/toolbar":27,"./modules/tooltip":28,"./modules/undo-manager":29,"./quill":30}],15:[function(_dereq_,module,exports){
var ColorPicker, Picker, dom,
  __hasProp = {}.hasOwnProperty,
  __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

dom = _dereq_('./dom');

Picker = _dereq_('./picker');

ColorPicker = (function(_super) {
  __extends(ColorPicker, _super);

  function ColorPicker() {
    ColorPicker.__super__.constructor.apply(this, arguments);
    dom(this.container).addClass('ql-color-picker');
  }

  ColorPicker.prototype.buildItem = function(picker, option, index) {
    var item;
    item = ColorPicker.__super__.buildItem.call(this, picker, option, index);
    item.style.backgroundColor = option.value;
    return item;
  };

  return ColorPicker;

})(Picker);

module.exports = ColorPicker;



},{"./dom":16,"./picker":19}],16:[function(_dereq_,module,exports){
var SelectWrapper, Wrapper, dom, lastKeyEvent, _,
  __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
  __hasProp = {}.hasOwnProperty,
  __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

_ = _dereq_('lodash');

lastKeyEvent = null;

Wrapper = (function() {
  function Wrapper(node) {
    this.node = node;
    this.trigger = __bind(this.trigger, this);
  }

  Wrapper.prototype.addClass = function(cssClass) {
    if (this.hasClass(cssClass)) {
      return;
    }
    if (this.node.classList != null) {
      this.node.classList.add(cssClass);
    } else if (this.node.className != null) {
      this.node.className = (this.node.className + ' ' + cssClass).trim();
    }
    return this;
  };

  Wrapper.prototype.attributes = function(attributes) {
    var attr, i, value, _i, _len, _ref;
    if (attributes) {
      _.each(attributes, (function(_this) {
        return function(value, name) {
          return _this.node.setAttribute(name, value);
        };
      })(this));
      return this;
    } else {
      if (this.node.attributes == null) {
        return {};
      }
      attributes = {};
      _ref = this.node.attributes;
      for (i = _i = 0, _len = _ref.length; _i < _len; i = ++_i) {
        value = _ref[i];
        attr = this.node.attributes[i];
        attributes[attr.name] = attr.value;
      }
      return attributes;
    }
  };

  Wrapper.prototype.child = function(offset) {
    var child, length;
    child = this.node.firstChild;
    length = dom(child).length();
    while (child != null) {
      if (offset < length) {
        break;
      }
      offset -= length;
      child = child.nextSibling;
      length = dom(child).length();
    }
    if (child == null) {
      child = this.node.lastChild;
      offset = dom(child).length();
    }
    return [child, offset];
  };

  Wrapper.prototype.childNodes = function() {
    return _.map(this.node.childNodes);
  };

  Wrapper.prototype.classes = function() {
    return this.node.className.split(/\s+/);
  };

  Wrapper.prototype.descendants = function() {
    return _.map(this.node.getElementsByTagName('*'));
  };

  Wrapper.prototype.get = function() {
    return this.node;
  };

  Wrapper.prototype.hasClass = function(cssClass) {
    if (this.node.classList != null) {
      return this.node.classList.contains(cssClass);
    } else if (this.node.className != null) {
      return this.classes().indexOf(cssClass) > -1;
    }
    return false;
  };

  Wrapper.prototype.isAncestor = function(ancestor, inclusive) {
    var node;
    if (inclusive == null) {
      inclusive = false;
    }
    if (ancestor === this.node) {
      return inclusive;
    }
    node = this.node;
    while (node) {
      if (node === ancestor) {
        return true;
      }
      node = node.parentNode;
    }
    return false;
  };

  Wrapper.prototype.isElement = function() {
    var _ref;
    return ((_ref = this.node) != null ? _ref.nodeType : void 0) === dom.ELEMENT_NODE;
  };

  Wrapper.prototype.isTextNode = function() {
    var _ref;
    return ((_ref = this.node) != null ? _ref.nodeType : void 0) === dom.TEXT_NODE;
  };

  Wrapper.prototype.length = function() {
    var length;
    if (this.node == null) {
      return 0;
    }
    length = this.text().length;
    if (this.isElement()) {
      length += this.node.querySelectorAll(Object.keys(dom.EMBED_TAGS).join(',')).length;
    }
    return length;
  };

  Wrapper.prototype.merge = function(node) {
    var $node;
    $node = dom(node);
    if (this.isElement()) {
      $node.moveChildren(this.node);
      this.normalize();
    } else {
      this.text(this.text() + $node.text());
    }
    $node.remove();
    return this;
  };

  Wrapper.prototype.moveChildren = function(newParent) {
    _.each(this.childNodes(), function(child) {
      return newParent.appendChild(child);
    });
    return this;
  };

  Wrapper.prototype.nextLineNode = function(root) {
    var nextNode;
    nextNode = this.node.nextSibling;
    if ((nextNode == null) && this.node.parentNode !== root) {
      nextNode = this.node.parentNode.nextSibling;
    }
    if ((nextNode != null) && (dom.LIST_TAGS[nextNode.tagName] != null)) {
      nextNode = nextNode.firstChild;
    }
    return nextNode;
  };

  Wrapper.prototype.normalize = function() {
    var $node, curNode, followingNode, nextNode;
    curNode = this.node.firstChild;
    while (curNode != null) {
      nextNode = curNode.nextSibling;
      $node = dom(curNode);
      if ((nextNode != null) && dom(nextNode).isTextNode()) {
        if ($node.text().length === 0) {
          $node.remove();
        } else if ($node.isTextNode()) {
          followingNode = nextNode.nextSibling;
          $node.merge(nextNode);
          nextNode = followingNode;
        }
      }
      curNode = nextNode;
    }
    return this;
  };

  Wrapper.prototype.on = function(eventName, listener) {
    this.node.addEventListener(eventName, (function(_this) {
      return function(event) {
        var arg, propagate;
        arg = lastKeyEvent && (eventName === 'keydown' || eventName === 'keyup') ? lastKeyEvent : event;
        propagate = listener.call(_this.node, arg);
        if (!propagate) {
          event.preventDefault();
          event.stopPropagation();
        }
        return propagate;
      };
    })(this));
    return this;
  };

  Wrapper.prototype.remove = function() {
    var _ref;
    if ((_ref = this.node.parentNode) != null) {
      _ref.removeChild(this.node);
    }
    this.node = null;
    return null;
  };

  Wrapper.prototype.removeClass = function(cssClass) {
    var classArray;
    if (!this.hasClass(cssClass)) {
      return;
    }
    if (this.node.classList != null) {
      this.node.classList.remove(cssClass);
    } else if (this.node.className != null) {
      classArray = this.classes();
      classArray.splice(classArray.indexOf(cssClass), 1);
      this.node.className = classArray.join(' ');
    }
    if (!this.node.getAttribute('class')) {
      this.node.removeAttribute('class');
    }
    return this;
  };

  Wrapper.prototype.replace = function(newNode) {
    this.node.parentNode.replaceChild(newNode, this.node);
    this.node = newNode;
    return newNode;
  };

  Wrapper.prototype.splitAncestors = function(root, force) {
    var nextNode, parentClone, parentNode, refNode;
    if (force == null) {
      force = false;
    }
    if (this.node === root || this.node.parentNode === root) {
      return this;
    }
    if ((this.node.previousSibling != null) || force) {
      parentNode = this.node.parentNode;
      parentClone = parentNode.cloneNode(false);
      parentNode.parentNode.insertBefore(parentClone, parentNode.nextSibling);
      refNode = this.node;
      while (refNode != null) {
        nextNode = refNode.nextSibling;
        parentClone.appendChild(refNode);
        refNode = nextNode;
      }
      return dom(parentClone).splitAncestors(root);
    } else {
      return dom(this.node.parentNode).splitAncestors(root);
    }
  };

  Wrapper.prototype.split = function(offset, force) {
    var after, child, childLeft, childRight, left, nextRight, nodeLength, right, _ref, _ref1;
    if (force == null) {
      force = false;
    }
    nodeLength = this.length();
    offset = Math.max(0, offset);
    offset = Math.min(offset, nodeLength);
    if (!(force || offset !== 0)) {
      return [this.node.previousSibling, this.node, false];
    }
    if (!(force || offset !== nodeLength)) {
      return [this.node, this.node.nextSibling, false];
    }
    if (this.node.nodeType === dom.TEXT_NODE) {
      after = this.node.splitText(offset);
      return [this.node, after, true];
    } else {
      left = this.node;
      right = this.node.cloneNode(false);
      this.node.parentNode.insertBefore(right, left.nextSibling);
      _ref = this.child(offset), child = _ref[0], offset = _ref[1];
      _ref1 = dom(child).split(offset), childLeft = _ref1[0], childRight = _ref1[1];
      while (childRight !== null) {
        nextRight = childRight.nextSibling;
        right.appendChild(childRight);
        childRight = nextRight;
      }
      return [left, right, true];
    }
  };

  Wrapper.prototype.styles = function(styles, overwrite) {
    var obj, styleString;
    if (overwrite == null) {
      overwrite = false;
    }
    if (styles) {
      if (!overwrite) {
        styles = _.defaults(styles, this.styles());
      }
      styleString = _.map(styles, function(style, name) {
        return "" + name + ": " + style;
      }).join('; ') + ';';
      this.node.setAttribute('style', styleString);
      return this;
    } else {
      styleString = this.node.getAttribute('style') || '';
      obj = _.reduce(styleString.split(';'), function(styles, str) {
        var name, value, _ref;
        _ref = str.split(':'), name = _ref[0], value = _ref[1];
        if (name && value) {
          name = name.trim();
          value = value.trim();
          styles[name.toLowerCase()] = value;
        }
        return styles;
      }, {});
      return obj;
    }
  };

  Wrapper.prototype.switchTag = function(newTag) {
    var attributes, newNode;
    newTag = newTag.toUpperCase();
    if (this.node.tagName === newTag) {
      return this;
    }
    newNode = document.createElement(newTag);
    attributes = this.attributes();
    if (dom.VOID_TAGS[newTag] == null) {
      this.moveChildren(newNode);
    }
    this.replace(newNode);
    return this.attributes(attributes).get();
  };

  Wrapper.prototype.text = function(text) {
    if (text != null) {
      switch (this.node.nodeType) {
        case dom.ELEMENT_NODE:
          this.node.textContent = text;
          break;
        case dom.TEXT_NODE:
          this.node.data = text;
      }
      return this;
    } else {
      switch (this.node.nodeType) {
        case dom.ELEMENT_NODE:
          if (this.node.tagName === dom.DEFAULT_BREAK_TAG) {
            return "";
          }
          if (dom.EMBED_TAGS[this.node.tagName] != null) {
            return dom.EMBED_TEXT;
          }
          if (this.node.textContent != null) {
            return this.node.textContent;
          }
          return "";
        case dom.TEXT_NODE:
          return this.node.data || "";
        default:
          return "";
      }
    }
  };

  Wrapper.prototype.textNodes = function() {
    var textNode, textNodes, walker;
    walker = document.createTreeWalker(this.node, NodeFilter.SHOW_TEXT, null, false);
    textNodes = [];
    while (textNode = walker.nextNode()) {
      textNodes.push(textNode);
    }
    return textNodes;
  };

  Wrapper.prototype.toggleClass = function(className, state) {
    if (state == null) {
      state = !this.hasClass(className);
    }
    if (state) {
      this.addClass(className);
    } else {
      this.removeClass(className);
    }
    return this;
  };

  Wrapper.prototype.trigger = function(eventName, options) {
    var event, initFn, modifiers;
    if (options == null) {
      options = {};
    }
    if (['keypress', 'keydown', 'keyup'].indexOf(eventName) < 0) {
      event = document.createEvent('Event');
      event.initEvent(eventName, options.bubbles, options.cancelable);
    } else {
      event = document.createEvent('KeyboardEvent');
      lastKeyEvent = _.clone(options);
      if (_.isNumber(options.key)) {
        lastKeyEvent.which = options.key;
      } else if (_.isString(options.key)) {
        lastKeyEvent.which = options.key.toUpperCase().charCodeAt(0);
      } else {
        lastKeyEvent.which = 0;
      }
      if (dom.isIE(10)) {
        modifiers = [];
        if (options.altKey) {
          modifiers.push('Alt');
        }
        if (options.ctrlKey) {
          modifiers.push('Control');
        }
        if (options.metaKey) {
          modifiers.push('Meta');
        }
        if (options.shiftKey) {
          modifiers.push('Shift');
        }
        event.initKeyboardEvent(eventName, options.bubbles, options.cancelable, window, 0, 0, modifiers.join(' '), null, null);
      } else {
        initFn = _.isFunction(event.initKeyboardEvent) ? 'initKeyboardEvent' : 'initKeyEvent';
        event[initFn](eventName, options.bubbles, options.cancelable, window, options.ctrlKey, options.altKey, options.shiftKey, options.metaKey, 0, 0);
      }
    }
    this.node.dispatchEvent(event);
    lastKeyEvent = null;
    return this;
  };

  Wrapper.prototype.unwrap = function() {
    var next, ret;
    ret = this.node.firstChild;
    next = this.node.nextSibling;
    _.each(this.childNodes(), (function(_this) {
      return function(child) {
        return _this.node.parentNode.insertBefore(child, next);
      };
    })(this));
    this.remove();
    return ret;
  };

  Wrapper.prototype.wrap = function(wrapper) {
    var parent;
    if (this.node.parentNode != null) {
      this.node.parentNode.insertBefore(wrapper, this.node);
    }
    parent = wrapper;
    while (parent.firstChild != null) {
      parent = wrapper.firstChild;
    }
    parent.appendChild(this.node);
    return this;
  };

  return Wrapper;

})();

SelectWrapper = (function(_super) {
  __extends(SelectWrapper, _super);

  function SelectWrapper() {
    return SelectWrapper.__super__.constructor.apply(this, arguments);
  }

  SelectWrapper.prototype["default"] = function() {
    return this.node.querySelector('option[selected]');
  };

  SelectWrapper.prototype.option = function(option, trigger) {
    var child, i, value, _i, _len, _ref;
    if (trigger == null) {
      trigger = true;
    }
    value = _.isElement(option) ? option.value : option;
    if (value) {
      value = value.replace(/[^\w]+/g, '');
      _ref = this.node.children;
      for (i = _i = 0, _len = _ref.length; _i < _len; i = ++_i) {
        child = _ref[i];
        if (child.value.replace(/[^\w]+/g, '') === value) {
          this.node.selectedIndex = i;
          break;
        }
      }
    } else {
      this.node.selectedIndex = -1;
    }
    if (trigger) {
      this.trigger('change');
    }
    return this;
  };

  SelectWrapper.prototype.reset = function(trigger) {
    var option;
    if (trigger == null) {
      trigger = true;
    }
    option = this["default"]();
    if (option != null) {
      option.selected = true;
    } else {
      this.node.selectedIndex = 0;
    }
    if (trigger) {
      this.trigger('change');
    }
    return this;
  };

  SelectWrapper.prototype.value = function() {
    if (this.node.selectedIndex > -1) {
      return this.node.options[this.node.selectedIndex].value;
    } else {
      return '';
    }
  };

  return SelectWrapper;

})(Wrapper);

dom = function(node) {
  if ((node != null ? node.tagName : void 0) === 'SELECT') {
    return new SelectWrapper(node);
  } else {
    return new Wrapper(node);
  }
};

dom = _.extend(dom, {
  ELEMENT_NODE: 1,
  NOBREAK_SPACE: "&nbsp;",
  TEXT_NODE: 3,
  ZERO_WIDTH_NOBREAK_SPACE: "\uFEFF",
  DEFAULT_BLOCK_TAG: 'DIV',
  DEFAULT_BREAK_TAG: 'BR',
  DEFAULT_INLINE_TAG: 'SPAN',
  EMBED_TEXT: '!',
  FONT_SIZES: {
    '10px': 1,
    '13px': 2,
    '16px': 3,
    '18px': 4,
    '24px': 5,
    '32px': 6,
    '48px': 7
  },
  KEYS: {
    BACKSPACE: 8,
    TAB: 9,
    ENTER: 13,
    ESCAPE: 27,
    LEFT: 37,
    UP: 38,
    RIGHT: 39,
    DOWN: 40,
    DELETE: 46
  },
  BLOCK_TAGS: {
    'ADDRESS': 'ADDRESS',
    'ARTICLE': 'ARTICLE',
    'ASIDE': 'ASIDE',
    'AUDIO': 'AUDIO',
    'BLOCKQUOTE': 'BLOCKQUOTE',
    'CANVAS': 'CANVAS',
    'DD': 'DD',
    'DIV': 'DIV',
    'DL': 'DL',
    'FIGCAPTION': 'FIGCAPTION',
    'FIGURE': 'FIGURE',
    'FOOTER': 'FOOTER',
    'FORM': 'FORM',
    'H1': 'H1',
    'H2': 'H2',
    'H3': 'H3',
    'H4': 'H4',
    'H5': 'H5',
    'H6': 'H6',
    'HEADER': 'HEADER',
    'HGROUP': 'HGROUP',
    'LI': 'LI',
    'OL': 'OL',
    'OUTPUT': 'OUTPUT',
    'P': 'P',
    'PRE': 'PRE',
    'SECTION': 'SECTION',
    'TABLE': 'TABLE',
    'TBODY': 'TBODY',
    'TD': 'TD',
    'TFOOT': 'TFOOT',
    'TH': 'TH',
    'THEAD': 'THEAD',
    'TR': 'TR',
    'UL': 'UL',
    'VIDEO': 'VIDEO'
  },
  EMBED_TAGS: {
    'IMG': 'IMG'
  },
  LINE_TAGS: {
    'DIV': 'DIV',
    'LI': 'LI'
  },
  LIST_TAGS: {
    'OL': 'OL',
    'UL': 'UL'
  },
  VOID_TAGS: {
    'AREA': 'AREA',
    'BASE': 'BASE',
    'BR': 'BR',
    'COL': 'COL',
    'COMMAND': 'COMMAND',
    'EMBED': 'EMBED',
    'HR': 'HR',
    'IMG': 'IMG',
    'INPUT': 'INPUT',
    'KEYGEN': 'KEYGEN',
    'LINK': 'LINK',
    'META': 'META',
    'PARAM': 'PARAM',
    'SOURCE': 'SOURCE',
    'TRACK': 'TRACK',
    'WBR': 'WBR'
  },
  convertFontSize: function(size) {
    var i, s, sources, targets;
    if (_.isString(size) && size.indexOf('px') > -1) {
      sources = Object.keys(dom.FONT_SIZES);
      targets = _.values(dom.FONT_SIZES);
    } else {
      targets = Object.keys(dom.FONT_SIZES);
      sources = _.values(dom.FONT_SIZES);
    }
    for (i in sources) {
      s = sources[i];
      if (parseInt(size) <= parseInt(s)) {
        return targets[i];
      }
    }
    return _.last(targets);
  },
  isIE: function(maxVersion) {
    var version;
    version = document.documentMode;
    return version && maxVersion >= version;
  },
  isIOS: function() {
    return /iPhone|iPad/i.test(navigator.userAgent);
  },
  isMac: function() {
    return /Mac/i.test(navigator.platform);
  }
});

module.exports = dom;



},{"lodash":1}],17:[function(_dereq_,module,exports){
var LinkedList, Node;

Node = (function() {
  function Node(data) {
    this.data = data;
    this.prev = this.next = null;
  }

  return Node;

})();

LinkedList = (function() {
  LinkedList.Node = Node;

  function LinkedList() {
    this.length = 0;
    this.first = this.last = null;
  }

  LinkedList.prototype.append = function(node) {
    if (this.first != null) {
      node.next = null;
      this.last.next = node;
    } else {
      this.first = node;
    }
    node.prev = this.last;
    this.last = node;
    return this.length += 1;
  };

  LinkedList.prototype.insertAfter = function(refNode, newNode) {
    newNode.prev = refNode;
    if (refNode != null) {
      newNode.next = refNode.next;
      if (refNode.next != null) {
        refNode.next.prev = newNode;
      }
      refNode.next = newNode;
      if (refNode === this.last) {
        this.last = newNode;
      }
    } else {
      newNode.next = this.first;
      this.first.prev = newNode;
      this.first = newNode;
    }
    return this.length += 1;
  };

  LinkedList.prototype.remove = function(node) {
    if (this.length > 1) {
      if (node.prev != null) {
        node.prev.next = node.next;
      }
      if (node.next != null) {
        node.next.prev = node.prev;
      }
      if (node === this.first) {
        this.first = node.next;
      }
      if (node === this.last) {
        this.last = node.prev;
      }
    } else {
      this.first = this.last = null;
    }
    node.prev = node.next = null;
    return this.length -= 1;
  };

  LinkedList.prototype.toArray = function() {
    var arr, cur;
    arr = [];
    cur = this.first;
    while (cur != null) {
      arr.push(cur);
      cur = cur.next;
    }
    return arr;
  };

  return LinkedList;

})();

module.exports = LinkedList;



},{}],18:[function(_dereq_,module,exports){
var Normalizer, dom, _;

_ = _dereq_('lodash');

dom = _dereq_('./dom');

Normalizer = {
  ALIASES: {
    'STRONG': 'B',
    'EM': 'I',
    'DEL': 'S',
    'STRIKE': 'S'
  },
  ATTRIBUTES: {
    'color': 'color',
    'face': 'fontFamily',
    'size': 'fontSize'
  },
  STYLES: {
    'background-color': 'background-color',
    'color': 'color',
    'font-family': 'font-family',
    'font-size': 'font-size',
    'text-align': 'text-align'
  },
  TAGS: {
    'DIV': 'DIV',
    'BR': 'BR',
    'SPAN': 'SPAN',
    'B': 'B',
    'I': 'I',
    'S': 'S',
    'U': 'U',
    'A': 'A',
    'IMG': 'IMG',
    'OL': 'OL',
    'UL': 'UL',
    'LI': 'LI'
  },
  handleBreaks: function(lineNode) {
    var breaks;
    breaks = _.map(lineNode.querySelectorAll(dom.DEFAULT_BREAK_TAG));
    _.each(breaks, (function(_this) {
      return function(br) {
        if ((br.nextSibling != null) && (!dom.isIE(10) || (br.previousSibling != null))) {
          return dom(br.nextSibling).splitAncestors(lineNode.parentNode);
        }
      };
    })(this));
    return lineNode;
  },
  normalizeLine: function(lineNode) {
    lineNode = Normalizer.wrapInline(lineNode);
    lineNode = Normalizer.handleBreaks(lineNode);
    lineNode = Normalizer.pullBlocks(lineNode);
    lineNode = Normalizer.normalizeNode(lineNode);
    Normalizer.unwrapText(lineNode);
    if ((lineNode != null) && (dom.LIST_TAGS[lineNode.tagName] != null)) {
      lineNode = lineNode.firstChild;
    }
    return lineNode;
  },
  normalizeNode: function(node) {
    if (dom(node).isTextNode()) {
      return node;
    }
    _.each(Normalizer.ATTRIBUTES, function(style, attribute) {
      var value;
      if (node.hasAttribute(attribute)) {
        value = node.getAttribute(attribute);
        if (attribute === 'size') {
          value = dom.convertFontSize(value);
        }
        node.style[style] = value;
        return node.removeAttribute(attribute);
      }
    });
    Normalizer.whitelistStyles(node);
    return Normalizer.whitelistTags(node);
  },
  optimizeLine: function(lineNode) {
    var lineNodeLength, node, nodes, _results;
    lineNode.normalize();
    lineNodeLength = dom(lineNode).length();
    nodes = dom(lineNode).descendants();
    _results = [];
    while (nodes.length > 0) {
      node = nodes.pop();
      if ((node != null ? node.parentNode : void 0) == null) {
        continue;
      }
      if (dom.EMBED_TAGS[node.tagName] != null) {
        continue;
      }
      if (node.tagName === dom.DEFAULT_BREAK_TAG) {
        if (lineNodeLength !== 0) {
          _results.push(dom(node).remove());
        } else {
          _results.push(void 0);
        }
      } else if (dom(node).length() === 0) {
        nodes.push(node.nextSibling);
        _results.push(dom(node).unwrap());
      } else if ((node.previousSibling != null) && node.tagName === node.previousSibling.tagName) {
        if (_.isEqual(dom(node).attributes(), dom(node.previousSibling).attributes())) {
          nodes.push(node.firstChild);
          _results.push(dom(node.previousSibling).merge(node));
        } else {
          _results.push(void 0);
        }
      } else {
        _results.push(void 0);
      }
    }
    return _results;
  },
  pullBlocks: function(lineNode) {
    var curNode;
    curNode = lineNode.firstChild;
    while (curNode != null) {
      if ((dom.BLOCK_TAGS[curNode.tagName] != null) && curNode.tagName !== 'LI') {
        if (curNode.previousSibling != null) {
          dom(curNode).splitAncestors(lineNode.parentNode);
        }
        if (curNode.nextSibling != null) {
          dom(curNode.nextSibling).splitAncestors(lineNode.parentNode);
        }
        if ((dom.LIST_TAGS[curNode.tagName] == null) || !curNode.firstChild) {
          dom(curNode).unwrap();
          Normalizer.pullBlocks(lineNode);
        } else {
          dom(curNode.parentNode).unwrap();
          if (lineNode.parentNode == null) {
            lineNode = curNode;
          }
        }
        break;
      }
      curNode = curNode.nextSibling;
    }
    return lineNode;
  },
  stripComments: function(html) {
    return html.replace(/<!--[\s\S]*?-->/g, '');
  },
  stripWhitespace: function(html) {
    html = html.trim();
    html = html.replace(/(\r?\n|\r)+/g, ' ');
    html = html.replace(/\>\s+\</g, '><');
    return html;
  },
  whitelistStyles: function(node) {
    var original, styles;
    original = dom(node).styles();
    styles = _.omit(original, function(value, key) {
      return Normalizer.STYLES[key] == null;
    });
    if (Object.keys(styles).length < Object.keys(original).length) {
      if (Object.keys(styles).length > 0) {
        return dom(node).styles(styles, true);
      } else {
        return node.removeAttribute('style');
      }
    }
  },
  whitelistTags: function(node) {
    if (!dom(node).isElement()) {
      return node;
    }
    if (Normalizer.ALIASES[node.tagName] != null) {
      node = dom(node).switchTag(Normalizer.ALIASES[node.tagName]);
    } else if (Normalizer.TAGS[node.tagName] == null) {
      if (dom.BLOCK_TAGS[node.tagName] != null) {
        node = dom(node).switchTag(dom.DEFAULT_BLOCK_TAG);
      } else if (!node.hasAttributes() && (node.firstChild != null)) {
        node = dom(node).unwrap();
      } else {
        node = dom(node).switchTag(dom.DEFAULT_INLINE_TAG);
      }
    }
    return node;
  },
  wrapInline: function(lineNode) {
    var blockNode, nextNode;
    if (dom.BLOCK_TAGS[lineNode.tagName] != null) {
      return lineNode;
    }
    blockNode = document.createElement(dom.DEFAULT_BLOCK_TAG);
    lineNode.parentNode.insertBefore(blockNode, lineNode);
    while ((lineNode != null) && (dom.BLOCK_TAGS[lineNode.tagName] == null)) {
      nextNode = lineNode.nextSibling;
      blockNode.appendChild(lineNode);
      lineNode = nextNode;
    }
    return blockNode;
  },
  unwrapText: function(lineNode) {
    var spans;
    spans = _.map(lineNode.querySelectorAll(dom.DEFAULT_INLINE_TAG));
    return _.each(spans, function(span) {
      if (!span.hasAttributes()) {
        return dom(span).unwrap();
      }
    });
  }
};

module.exports = Normalizer;



},{"./dom":16,"lodash":1}],19:[function(_dereq_,module,exports){
var Normalizer, Picker, dom, _;

_ = _dereq_('lodash');

dom = _dereq_('./dom');

Normalizer = _dereq_('./normalizer');

Picker = (function() {
  Picker.TEMPLATE = '<span class="ql-picker-label"></span><span class="ql-picker-options"></span>';

  function Picker(select) {
    this.select = select;
    this.container = document.createElement('span');
    this.buildPicker();
    dom(this.container).addClass('ql-picker');
    this.select.style.display = 'none';
    this.select.parentNode.insertBefore(this.container, this.select);
    dom(document).on('click', (function(_this) {
      return function() {
        _this.close();
        return true;
      };
    })(this));
    dom(this.label).on('click', (function(_this) {
      return function() {
        _.defer(function() {
          return dom(_this.container).toggleClass('ql-expanded');
        });
        return false;
      };
    })(this));
    dom(this.select).on('change', (function(_this) {
      return function() {
        var item, option;
        if (_this.select.selectedIndex > -1) {
          item = _this.container.querySelectorAll('.ql-picker-item')[_this.select.selectedIndex];
          option = _this.select.options[_this.select.selectedIndex];
        }
        _this.selectItem(item, false);
        return dom(_this.label).toggleClass('ql-active', option !== dom(_this.select)["default"]());
      };
    })(this));
  }

  Picker.prototype.buildItem = function(picker, option, index) {
    var item;
    item = document.createElement('span');
    item.setAttribute('data-value', option.getAttribute('value'));
    dom(item).addClass('ql-picker-item').text(dom(option).text()).on('click', (function(_this) {
      return function() {
        _this.selectItem(item, true);
        return _this.close();
      };
    })(this));
    if (this.select.selectedIndex === index) {
      this.selectItem(item, false);
    }
    return item;
  };

  Picker.prototype.buildPicker = function() {
    var picker;
    _.each(dom(this.select).attributes(), (function(_this) {
      return function(value, name) {
        return _this.container.setAttribute(name, value);
      };
    })(this));
    this.container.innerHTML = Normalizer.stripWhitespace(Picker.TEMPLATE);
    this.label = this.container.querySelector('.ql-picker-label');
    picker = this.container.querySelector('.ql-picker-options');
    return _.each(this.select.options, (function(_this) {
      return function(option, i) {
        var item;
        item = _this.buildItem(picker, option, i);
        return picker.appendChild(item);
      };
    })(this));
  };

  Picker.prototype.close = function() {
    return dom(this.container).removeClass('ql-expanded');
  };

  Picker.prototype.selectItem = function(item, trigger) {
    var selected, value;
    selected = this.container.querySelector('.ql-selected');
    if (selected != null) {
      dom(selected).removeClass('ql-selected');
    }
    if (item != null) {
      value = item.getAttribute('data-value');
      dom(item).addClass('ql-selected');
      dom(this.label).text(dom(item).text());
      dom(this.select).option(value, trigger);
      return this.label.setAttribute('data-value', value);
    } else {
      this.label.innerHTML = '&nbsp;';
      return this.label.removeAttribute('data-value');
    }
  };

  return Picker;

})();

module.exports = Picker;



},{"./dom":16,"./normalizer":18,"lodash":1}],20:[function(_dereq_,module,exports){
var Range, _;

_ = _dereq_('lodash');

Range = (function() {
  Range.compare = function(r1, r2) {
    if (r1 === r2) {
      return true;
    }
    if (!((r1 != null) && (r2 != null))) {
      return false;
    }
    return r1.equals(r2);
  };

  function Range(start, end) {
    this.start = start;
    this.end = end;
  }

  Range.prototype.equals = function(range) {
    if (range == null) {
      return false;
    }
    return this.start === range.start && this.end === range.end;
  };

  Range.prototype.shift = function(index, length) {
    var _ref;
    return _ref = _.map([this.start, this.end], function(pos) {
      if (index > pos) {
        return pos;
      }
      if (length >= 0) {
        return pos + length;
      } else {
        return Math.max(index, pos + length);
      }
    }), this.start = _ref[0], this.end = _ref[1], _ref;
  };

  Range.prototype.isCollapsed = function() {
    return this.start === this.end;
  };

  return Range;

})();

module.exports = Range;



},{"lodash":1}],21:[function(_dereq_,module,exports){
var Authorship, Delta, Quill, dom, _;

Quill = _dereq_('../quill');

_ = Quill.require('lodash');

dom = Quill.require('dom');

Delta = Quill.require('delta');

Authorship = (function() {
  Authorship.DEFAULTS = {
    authorId: null,
    color: 'transparent',
    enabled: false
  };

  function Authorship(quill, options) {
    this.quill = quill;
    this.options = options;
    if (this.options.button != null) {
      this.attachButton(this.options.button);
    }
    if (this.options.enabled) {
      this.enable();
    }
    this.quill.addFormat('author', {
      "class": 'author-'
    });
    if (this.options.authorId == null) {
      return;
    }
    this.quill.on(this.quill.constructor.events.PRE_EVENT, (function(_this) {
      return function(eventName, delta, origin) {
        var authorDelta, authorFormat;
        if (eventName === _this.quill.constructor.events.TEXT_CHANGE && origin === 'user') {
          authorDelta = new Delta();
          authorFormat = {
            author: _this.options.authorId
          };
          _.each(delta.ops, function(op) {
            if (op["delete"] != null) {
              return;
            }
            if ((op.insert != null) || ((op.retain != null) && (op.attributes != null))) {
              op.attributes || (op.attributes = {});
              op.attributes.author = _this.options.authorId;
              return authorDelta.retain(op.retain || op.insert.length || 1, authorFormat);
            } else {
              return authorDelta.retain(op.retain);
            }
          });
          return _this.quill.updateContents(authorDelta, Quill.sources.SILENT);
        }
      };
    })(this));
    this.addAuthor(this.options.authorId, this.options.color);
  }

  Authorship.prototype.addAuthor = function(id, color) {
    var styles;
    styles = {};
    styles[".authorship .author-" + id] = {
      "background-color": "" + color
    };
    return this.quill.theme.addStyles(styles);
  };

  Authorship.prototype.attachButton = function(button) {
    var $button;
    $button = dom(button);
    return $button.on('click', (function(_this) {
      return function() {
        $button.toggleClass('ql-on');
        return _this.enable($dom.hasClass('ql-on'));
      };
    })(this));
  };

  Authorship.prototype.enable = function(enabled) {
    if (enabled == null) {
      enabled = true;
    }
    return dom(this.quill.root).toggleClass('authorship', enabled);
  };

  Authorship.prototype.disable = function() {
    return this.enable(false);
  };

  return Authorship;

})();

Quill.registerModule('authorship', Authorship);

module.exports = Authorship;



},{"../quill":30}],22:[function(_dereq_,module,exports){
var Delta, ImageTooltip, Quill, Tooltip, dom, _,
  __hasProp = {}.hasOwnProperty,
  __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

Quill = _dereq_('../quill');

Tooltip = _dereq_('./tooltip');

_ = Quill.require('lodash');

dom = Quill.require('dom');

Delta = Quill.require('delta');

ImageTooltip = (function(_super) {
  __extends(ImageTooltip, _super);

  ImageTooltip.DEFAULTS = {
    template: '<input class="input" type="textbox"> <div class="preview"> <span>Preview</span> </div> <a href="javascript:;" class="cancel">Cancel</a> <a href="javascript:;" class="insert">Insert</a>'
  };

  function ImageTooltip(quill, options) {
    this.quill = quill;
    this.options = options;
    this.options = _.defaults(this.options, Tooltip.DEFAULTS);
    ImageTooltip.__super__.constructor.call(this, this.quill, this.options);
    this.preview = this.container.querySelector('.preview');
    this.textbox = this.container.querySelector('.input');
    dom(this.container).addClass('ql-image-tooltip');
    this.initListeners();
  }

  ImageTooltip.prototype.initListeners = function() {
    dom(this.container.querySelector('.insert')).on('click', _.bind(this.insertImage, this));
    dom(this.container.querySelector('.cancel')).on('click', _.bind(this.hide, this));
    dom(this.textbox).on('input', _.bind(this._preview, this));
    this.initTextbox(this.textbox, this.insertImage, this.hide);
    return this.quill.onModuleLoad('toolbar', (function(_this) {
      return function(toolbar) {
        return toolbar.initFormat('image', _.bind(_this._onToolbar, _this));
      };
    })(this));
  };

  ImageTooltip.prototype.insertImage = function() {
    var index, url;
    url = this._normalizeURL(this.textbox.value);
    if (this.range == null) {
      this.range = new Range(0, 0);
    }
    if (this.range) {
      this.preview.innerHTML = '<span>Preview</span>';
      this.textbox.value = '';
      index = this.range.end;
      this.quill.insertEmbed(index, 'image', url, 'user');
      this.quill.setSelection(index + 1, index + 1);
    }
    return this.hide();
  };

  ImageTooltip.prototype._onToolbar = function(range, value) {
    if (value) {
      if (!this.textbox.value) {
        this.textbox.value = 'http://';
      }
      this.show();
      this.textbox.focus();
      return _.defer((function(_this) {
        return function() {
          return _this.textbox.setSelectionRange(_this.textbox.value.length, _this.textbox.value.length);
        };
      })(this));
    } else {
      return this.quill.deleteText(range, 'user');
    }
  };

  ImageTooltip.prototype._preview = function() {
    var img;
    if (!this._matchImageURL(this.textbox.value)) {
      return;
    }
    if (this.preview.firstChild.tagName === 'IMG') {
      return this.preview.firstChild.setAttribute('src', this.textbox.value);
    } else {
      img = document.createElement('img');
      img.setAttribute('src', this.textbox.value);
      return this.preview.replaceChild(img, this.preview.firstChild);
    }
  };

  ImageTooltip.prototype._matchImageURL = function(url) {
    return /^https?:\/\/.+\.(jpe?g|gif|png)$/.test(url);
  };

  ImageTooltip.prototype._normalizeURL = function(url) {
    if (!/^https?:\/\//.test(url)) {
      url = 'http://' + url;
    }
    return url;
  };

  return ImageTooltip;

})(Tooltip);

Quill.registerModule('image-tooltip', ImageTooltip);

module.exports = ImageTooltip;



},{"../quill":30,"./tooltip":28}],23:[function(_dereq_,module,exports){
var Delta, Keyboard, Quill, dom, _;

Quill = _dereq_('../quill');

_ = Quill.require('lodash');

dom = Quill.require('dom');

Delta = Quill.require('delta');

Keyboard = (function() {
  Keyboard.hotkeys = {
    BOLD: {
      key: 'B',
      metaKey: true
    },
    INDENT: {
      key: dom.KEYS.TAB
    },
    ITALIC: {
      key: 'I',
      metaKey: true
    },
    OUTDENT: {
      key: dom.KEYS.TAB,
      shiftKey: true
    },
    UNDERLINE: {
      key: 'U',
      metaKey: true
    }
  };

  function Keyboard(quill, options) {
    this.quill = quill;
    this.hotkeys = {};
    this._initListeners();
    this._initHotkeys();
    this._initDeletes();
  }

  Keyboard.prototype.addHotkey = function(hotkeys, callback) {
    if (!Array.isArray(hotkeys)) {
      hotkeys = [hotkeys];
    }
    return _.each(hotkeys, (function(_this) {
      return function(hotkey) {
        var which, _base;
        hotkey = _.isObject(hotkey) ? _.clone(hotkey) : {
          key: hotkey
        };
        hotkey.callback = callback;
        which = _.isNumber(hotkey.key) ? hotkey.key : hotkey.key.toUpperCase().charCodeAt(0);
        if ((_base = _this.hotkeys)[which] == null) {
          _base[which] = [];
        }
        return _this.hotkeys[which].push(hotkey);
      };
    })(this));
  };

  Keyboard.prototype.toggleFormat = function(range, format) {
    var delta, toolbar, value;
    if (range.isCollapsed()) {
      delta = this.quill.getContents(Math.max(0, range.start - 1), range.end);
    } else {
      delta = this.quill.getContents(range);
    }
    value = delta.ops.length === 0 || !_.all(delta.ops, function(op) {
      var _ref;
      return (_ref = op.attributes) != null ? _ref[format] : void 0;
    });
    if (range.isCollapsed()) {
      this.quill.prepareFormat(format, value);
    } else {
      this.quill.formatText(range, format, value, Quill.sources.USER);
    }
    toolbar = this.quill.getModule('toolbar');
    if (toolbar != null) {
      return toolbar.setActive(format, value);
    }
  };

  Keyboard.prototype._initDeletes = function() {
    return this.addHotkey([dom.KEYS.DELETE, dom.KEYS.BACKSPACE], (function(_this) {
      return function(range, hotkey) {
        var start;
        if ((range != null) && _this.quill.getLength() > 1) {
          if (range.start !== range.end) {
            _this.quill.deleteText(range.start, range.end, Quill.sources.USER);
          } else {
            start = hotkey.key === dom.KEYS.BACKSPACE ? range.start - 1 : range.start;
            if (start >= 0) {
              _this.quill.deleteText(start, start + 1, Quill.sources.USER);
            }
          }
        }
        return false;
      };
    })(this));
  };

  Keyboard.prototype._initHotkeys = function() {
    this.addHotkey(Keyboard.hotkeys.INDENT, (function(_this) {
      return function(range) {
        _this._onTab(range, false);
        return false;
      };
    })(this));
    this.addHotkey(Keyboard.hotkeys.OUTDENT, (function(_this) {
      return function(range) {
        return false;
      };
    })(this));
    return _.each(['bold', 'italic', 'underline'], (function(_this) {
      return function(format) {
        return _this.addHotkey(Keyboard.hotkeys[format.toUpperCase()], function(range) {
          _this.toggleFormat(range, format);
          return false;
        });
      };
    })(this));
  };

  Keyboard.prototype._initListeners = function() {
    return dom(this.quill.root).on('keydown', (function(_this) {
      return function(event) {
        var prevent;
        prevent = false;
        _.each(_this.hotkeys[event.which], function(hotkey) {
          var metaKey;
          metaKey = dom.isMac() ? event.metaKey : event.metaKey || event.ctrlKey;
          if (!!hotkey.metaKey !== !!metaKey) {
            return;
          }
          if (!!hotkey.shiftKey !== !!event.shiftKey) {
            return;
          }
          if (!!hotkey.altKey !== !!event.altKey) {
            return;
          }
          prevent = hotkey.callback(_this.quill.getSelection(), hotkey, event) === false || prevent;
          return true;
        });
        return !prevent;
      };
    })(this));
  };

  Keyboard.prototype._onTab = function(range, shift) {
    var delta;
    if (shift == null) {
      shift = false;
    }
    delta = new Delta().retain(range.start).insert("\t")["delete"](range.end - range.start).retain(this.quill.getLength() - range.end);
    this.quill.updateContents(delta, Quill.sources.USER);
    return this.quill.setSelection(range.start + 1, range.start + 1);
  };

  return Keyboard;

})();

Quill.registerModule('keyboard', Keyboard);

module.exports = Keyboard;



},{"../quill":30}],24:[function(_dereq_,module,exports){
var LinkTooltip, Quill, Tooltip, dom, _,
  __hasProp = {}.hasOwnProperty,
  __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

Quill = _dereq_('../quill');

Tooltip = _dereq_('./tooltip');

_ = Quill.require('lodash');

dom = Quill.require('dom');

LinkTooltip = (function(_super) {
  __extends(LinkTooltip, _super);

  LinkTooltip.DEFAULTS = {
    maxLength: 50,
    template: '<span class="title">Visit URL:&nbsp;</span> <a href="#" class="url" target="_blank" href="about:blank"></a> <input class="input" type="text"> <span>&nbsp;&#45;&nbsp;</span> <a href="javascript:;" class="change">Change</a> <a href="javascript:;" class="done">Done</a>'
  };

  function LinkTooltip(quill, options) {
    this.quill = quill;
    this.options = options;
    this.options = _.defaults(this.options, Tooltip.DEFAULTS);
    LinkTooltip.__super__.constructor.call(this, this.quill, this.options);
    dom(this.container).addClass('ql-link-tooltip');
    this.textbox = this.container.querySelector('.input');
    this.link = this.container.querySelector('.url');
    this.initListeners();
  }

  LinkTooltip.prototype.initListeners = function() {
    this.quill.on(this.quill.constructor.events.SELECTION_CHANGE, (function(_this) {
      return function(range) {
        var anchor;
        if (!((range != null) && range.isCollapsed())) {
          return;
        }
        anchor = _this._findAnchor(range);
        if (anchor) {
          _this.setMode(anchor.href, false);
          return _this.show(anchor);
        } else {
          _this.range = null;
          return _this.hide();
        }
      };
    })(this));
    dom(this.container.querySelector('.done')).on('click', _.bind(this.saveLink, this));
    dom(this.container.querySelector('.change')).on('click', (function(_this) {
      return function() {
        return _this.setMode(_this.link.href, true);
      };
    })(this));
    this.initTextbox(this.textbox, this.saveLink, this.hide);
    return this.quill.onModuleLoad('toolbar', (function(_this) {
      return function(toolbar) {
        return toolbar.initFormat('link', _.bind(_this._onToolbar, _this));
      };
    })(this));
  };

  LinkTooltip.prototype.saveLink = function() {
    var anchor, url;
    url = this._normalizeURL(this.textbox.value);
    if (this.range != null) {
      if (this.range.isCollapsed()) {
        anchor = this._findAnchor(this.range);
        if (anchor != null) {
          anchor.href = url;
        }
      } else {
        this.quill.formatText(this.range, 'link', url, 'user');
      }
    }
    return this.setMode(url, false);
  };

  LinkTooltip.prototype.setMode = function(url, edit) {
    var text;
    if (edit == null) {
      edit = false;
    }
    if (edit) {
      this.textbox.value = url;
      _.defer((function(_this) {
        return function() {
          _this.textbox.focus();
          return _this.textbox.setSelectionRange(url.length, url.length);
        };
      })(this));
    } else {
      this.link.href = url;
      text = url.length > this.options.maxLength ? url.slice(0, this.options.maxLength) + '...' : url;
      dom(this.link).text(text);
    }
    return dom(this.container).toggleClass('editing', edit);
  };

  LinkTooltip.prototype._findAnchor = function(range) {
    var leaf, node, offset, _ref;
    _ref = this.quill.editor.doc.findLeafAt(range.start, true), leaf = _ref[0], offset = _ref[1];
    if (leaf != null) {
      node = leaf.node;
    }
    while (node != null) {
      if (node.tagName === 'A') {
        return node;
      }
      node = node.parentNode;
    }
    return null;
  };

  LinkTooltip.prototype._onToolbar = function(range, value) {
    var nativeRange;
    if (!(range && !range.isCollapsed())) {
      return;
    }
    if (value) {
      this.setMode(this._suggestURL(range), true);
      nativeRange = this.quill.editor.selection._getNativeRange();
      return this.show(nativeRange);
    } else {
      return this.quill.formatText(range, 'link', false, 'user');
    }
  };

  LinkTooltip.prototype._normalizeURL = function(url) {
    if (!/^(https?:\/\/|mailto:)/.test(url)) {
      url = 'http://' + url;
    }
    return url;
  };

  LinkTooltip.prototype._suggestURL = function(range) {
    var text;
    text = this.quill.getText(range);
    return this._normalizeURL(text);
  };

  return LinkTooltip;

})(Tooltip);

Quill.registerModule('link-tooltip', LinkTooltip);

module.exports = LinkTooltip;



},{"../quill":30,"./tooltip":28}],25:[function(_dereq_,module,exports){
var EventEmitter2, MultiCursor, Quill, dom, _,
  __hasProp = {}.hasOwnProperty,
  __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

Quill = _dereq_('../quill');

EventEmitter2 = _dereq_('eventemitter2').EventEmitter2;

_ = Quill.require('lodash');

dom = Quill.require('dom');

MultiCursor = (function(_super) {
  __extends(MultiCursor, _super);

  MultiCursor.DEFAULTS = {
    template: '<span class="cursor-flag"> <span class="cursor-name"></span> </span> <span class="cursor-caret"></span>',
    timeout: 2500
  };

  MultiCursor.events = {
    CURSOR_ADDED: 'cursor-addded',
    CURSOR_MOVED: 'cursor-moved',
    CURSOR_REMOVED: 'cursor-removed'
  };

  function MultiCursor(quill, options) {
    this.quill = quill;
    this.options = options;
    this.cursors = {};
    this.container = this.quill.addContainer('ql-multi-cursor', true);
    this.quill.on(this.quill.constructor.events.TEXT_CHANGE, _.bind(this._applyDelta, this));
  }

  MultiCursor.prototype.clearCursors = function() {
    _.each(Object.keys(this.cursors), _.bind(this.removeCursor, this));
    return this.cursors = {};
  };

  MultiCursor.prototype.moveCursor = function(userId, index) {
    var cursor;
    cursor = this.cursors[userId];
    cursor.index = index;
    dom(cursor.elem).removeClass('hidden');
    clearTimeout(cursor.timer);
    cursor.timer = setTimeout((function(_this) {
      return function() {
        dom(cursor.elem).addClass('hidden');
        return cursor.timer = null;
      };
    })(this), this.options.timeout);
    this._updateCursor(cursor);
    return cursor;
  };

  MultiCursor.prototype.removeCursor = function(userId) {
    var cursor;
    cursor = this.cursors[userId];
    this.emit(MultiCursor.events.CURSOR_REMOVED, cursor);
    if (cursor != null) {
      cursor.elem.parentNode.removeChild(cursor.elem);
    }
    return delete this.cursors[userId];
  };

  MultiCursor.prototype.setCursor = function(userId, index, name, color) {
    var cursor;
    if (this.cursors[userId] == null) {
      this.cursors[userId] = cursor = {
        userId: userId,
        index: index,
        color: color,
        elem: this._buildCursor(name, color)
      };
      this.emit(MultiCursor.events.CURSOR_ADDED, cursor);
    }
    _.defer((function(_this) {
      return function() {
        return _this.moveCursor(userId, index);
      };
    })(this));
    return this.cursors[userId];
  };

  MultiCursor.prototype.shiftCursors = function(index, length, authorId) {
    if (authorId == null) {
      authorId = null;
    }
    return _.each(this.cursors, (function(_this) {
      return function(cursor, id) {
        if (!(cursor && (cursor.index > index || cursor.userId === authorId))) {
          return;
        }
        return cursor.index += Math.max(length, index - cursor.index);
      };
    })(this));
  };

  MultiCursor.prototype.update = function() {
    return _.each(this.cursors, (function(_this) {
      return function(cursor, id) {
        if (cursor == null) {
          return;
        }
        _this._updateCursor(cursor);
        return true;
      };
    })(this));
  };

  MultiCursor.prototype._applyDelta = function(delta) {
    var index;
    index = 0;
    _.each(delta.ops, (function(_this) {
      return function(op) {
        var length, _ref;
        length = 0;
        if (op.insert != null) {
          length = op.insert.length || 1;
          _this.shiftCursors(index, length, (_ref = op.attributes) != null ? _ref['author'] : void 0);
        } else if (op["delete"] != null) {
          _this.shiftCursors(index, -1 * op["delete"], null);
        } else if (op.retain != null) {
          _this.shiftCursors(index, 0, null);
          length = op.retain;
        }
        return index += length;
      };
    })(this));
    return this.update();
  };

  MultiCursor.prototype._buildCursor = function(name, color) {
    var cursor, cursorCaret, cursorFlag, cursorName;
    cursor = document.createElement('span');
    dom(cursor).addClass('cursor');
    cursor.innerHTML = this.options.template;
    cursorFlag = cursor.querySelector('.cursor-flag');
    cursorName = cursor.querySelector('.cursor-name');
    dom(cursorName).text(name);
    cursorCaret = cursor.querySelector('.cursor-caret');
    cursorCaret.style.backgroundColor = cursorName.style.backgroundColor = color;
    this.container.appendChild(cursor);
    return cursor;
  };

  MultiCursor.prototype._updateCursor = function(cursor) {
    var bounds, flag;
    bounds = this.quill.getBounds(cursor.index);
    cursor.elem.style.top = (bounds.top - this.quill.container.scrollTop) + 'px';
    cursor.elem.style.left = bounds.left + 'px';
    cursor.elem.style.height = bounds.height + 'px';
    flag = cursor.elem.querySelector('.cursor-flag');
    dom(cursor.elem).toggleClass('top', parseInt(cursor.elem.style.top) <= flag.offsetHeight).toggleClass('left', parseInt(cursor.elem.style.left) <= flag.offsetWidth).toggleClass('right', this.quill.root.offsetWidth - parseInt(cursor.elem.style.left) <= flag.offsetWidth);
    return this.emit(MultiCursor.events.CURSOR_MOVED, cursor);
  };

  return MultiCursor;

})(EventEmitter2);

Quill.registerModule('multi-cursor', MultiCursor);

module.exports = MultiCursor;



},{"../quill":30,"eventemitter2":2}],26:[function(_dereq_,module,exports){
var Delta, Document, PasteManager, Quill, dom, _;

Quill = _dereq_('../quill');

Document = _dereq_('../core/document');

_ = Quill.require('lodash');

dom = Quill.require('dom');

Delta = Quill.require('delta');

PasteManager = (function() {
  function PasteManager(quill, options) {
    this.quill = quill;
    this.options = options;
    this.container = this.quill.addContainer('ql-paste-manager');
    this.container.setAttribute('contenteditable', true);
    dom(this.quill.root).on('paste', _.bind(this._paste, this));
  }

  PasteManager.prototype._paste = function() {
    var oldDocLength, range;
    oldDocLength = this.quill.getLength();
    range = this.quill.getSelection();
    if (range == null) {
      return;
    }
    this.container.focus();
    return _.defer((function(_this) {
      return function() {
        var delta, doc, lengthAdded, line, lineBottom, offset, windowBottom, _ref;
        doc = new Document(_this.container, _this.quill.options);
        delta = doc.toDelta();
        lengthAdded = delta.length() - 1;
        delta.compose(new Delta().retain(lengthAdded)["delete"](1));
        if (range.start > 0) {
          delta.ops.unshift({
            retain: range.start
          });
        }
        delta["delete"](range.end - range.start);
        _this.quill.updateContents(delta, 'user');
        _this.quill.setSelection(range.start + lengthAdded, range.start + lengthAdded);
        _ref = _this.quill.editor.doc.findLineAt(range.start + lengthAdded), line = _ref[0], offset = _ref[1];
        lineBottom = line.node.getBoundingClientRect().bottom;
        windowBottom = document.documentElement.clientHeight;
        if (lineBottom > windowBottom) {
          line.node.scrollIntoView(false);
        }
        return _this.container.innerHTML = "";
      };
    })(this));
  };

  return PasteManager;

})();

Quill.registerModule('paste-manager', PasteManager);

module.exports = PasteManager;



},{"../core/document":8,"../quill":30}],27:[function(_dereq_,module,exports){
var Quill, Toolbar, dom, _;

Quill = _dereq_('../quill');

_ = Quill.require('lodash');

dom = Quill.require('dom');

Toolbar = (function() {
  Toolbar.DEFAULTS = {
    container: null
  };

  Toolbar.formats = {
    LINE: {
      'align': 'align',
      'bullet': 'bullet',
      'list': 'list'
    },
    SELECT: {
      'align': 'align',
      'background': 'background',
      'color': 'color',
      'font': 'font',
      'size': 'size'
    },
    TOGGLE: {
      'bold': 'bold',
      'bullet': 'bullet',
      'image': 'image',
      'italic': 'italic',
      'link': 'link',
      'list': 'list',
      'strike': 'strike',
      'underline': 'underline'
    },
    TOOLTIP: {
      'image': 'image',
      'link': 'link'
    }
  };

  function Toolbar(quill, options) {
    this.quill = quill;
    this.options = options;
    if (_.isString(this.options) || _.isElement(this.options)) {
      this.options = {
        container: this.options
      };
    }
    if (this.options.container == null) {
      throw new Error('container required for toolbar', this.options);
    }
    this.container = _.isString(this.options.container) ? document.querySelector(this.options.container) : this.options.container;
    this.inputs = {};
    this.preventUpdate = false;
    this.triggering = false;
    _.each(this.quill.options.formats, (function(_this) {
      return function(format) {
        if (Toolbar.formats.TOOLTIP[format] != null) {
          return;
        }
        return _this.initFormat(format, function(range, value) {
          if (_this.triggering) {
            return;
          }
          if (range.isCollapsed()) {
            _this.quill.prepareFormat(format, value);
          } else if (Toolbar.formats.LINE[format] != null) {
            _this.quill.formatLine(range, format, value, 'user');
          } else {
            _this.quill.formatText(range, format, value, 'user');
          }
          return _.defer(function() {
            _this.updateActive(range, ['bullet', 'list']);
            return _this.setActive(format, value);
          });
        });
      };
    })(this));
    this.quill.on(this.quill.constructor.events.SELECTION_CHANGE, (function(_this) {
      return function(range) {
        if (range != null) {
          return _this.updateActive(range);
        }
      };
    })(this));
    this.quill.onModuleLoad('keyboard', (function(_this) {
      return function(keyboard) {
        return keyboard.addHotkey([dom.KEYS.BACKSPACE, dom.KEYS.DELETE, dom.KEYS.ENTER], function() {
          return _.defer(_.bind(_this.updateActive, _this));
        });
      };
    })(this));
    dom(this.container).addClass('ql-toolbar');
    if (dom.isIOS()) {
      dom(this.container).addClass('ios');
    }
    if (dom.isIE(11)) {
      dom(this.container).on('mousedown', (function(_this) {
        return function() {
          return false;
        };
      })(this));
    }
  }

  Toolbar.prototype.initFormat = function(format, callback) {
    var eventName, input, selector;
    selector = ".ql-" + format;
    if (Toolbar.formats.SELECT[format] != null) {
      selector = "select" + selector;
      eventName = 'change';
    } else {
      eventName = 'click';
    }
    input = this.container.querySelector(selector);
    if (input == null) {
      return;
    }
    this.inputs[format] = input;
    return dom(input).on(eventName, (function(_this) {
      return function() {
        var range, value;
        value = eventName === 'change' ? dom(input).value() : !dom(input).hasClass('ql-active');
        _this.preventUpdate = true;
        _this.quill.focus();
        range = _this.quill.getSelection();
        if (range != null) {
          callback(range, value);
        }
        _this.preventUpdate = false;
        return true;
      };
    })(this));
  };

  Toolbar.prototype.setActive = function(format, value) {
    var $input, input, selectValue, _ref;
    input = this.inputs[format];
    if (input == null) {
      return;
    }
    $input = dom(input);
    if (input.tagName === 'SELECT') {
      this.triggering = true;
      selectValue = $input.value(input);
      if (value == null) {
        value = (_ref = $input["default"]()) != null ? _ref.value : void 0;
      }
      if (Array.isArray(value)) {
        value = '';
      }
      if (value !== selectValue) {
        if (value != null) {
          $input.option(value);
        } else {
          $input.reset();
        }
      }
      return this.triggering = false;
    } else {
      return $input.toggleClass('ql-active', value || false);
    }
  };

  Toolbar.prototype.updateActive = function(range, formats) {
    var activeFormats;
    if (formats == null) {
      formats = null;
    }
    range || (range = this.quill.getSelection());
    if (!((range != null) && !this.preventUpdate)) {
      return;
    }
    activeFormats = this._getActive(range);
    return _.each(this.inputs, (function(_this) {
      return function(input, format) {
        if (!Array.isArray(formats) || formats.indexOf(format) > -1) {
          _this.setActive(format, activeFormats[format]);
        }
        return true;
      };
    })(this));
  };

  Toolbar.prototype._getActive = function(range) {
    var leafFormats, lineFormats;
    leafFormats = this._getLeafActive(range);
    lineFormats = this._getLineActive(range);
    return _.defaults({}, leafFormats, lineFormats);
  };

  Toolbar.prototype._getLeafActive = function(range) {
    var contents, formatsArr, line, offset, _ref;
    if (range.isCollapsed()) {
      _ref = this.quill.editor.doc.findLineAt(range.start), line = _ref[0], offset = _ref[1];
      if (offset === 0) {
        contents = this.quill.getContents(range.start, range.end + 1);
      } else {
        contents = this.quill.getContents(range.start - 1, range.end);
      }
    } else {
      contents = this.quill.getContents(range);
    }
    formatsArr = _.map(contents.ops, 'attributes');
    return this._intersectFormats(formatsArr);
  };

  Toolbar.prototype._getLineActive = function(range) {
    var firstLine, formatsArr, lastLine, offset, _ref, _ref1;
    formatsArr = [];
    _ref = this.quill.editor.doc.findLineAt(range.start), firstLine = _ref[0], offset = _ref[1];
    _ref1 = this.quill.editor.doc.findLineAt(range.end), lastLine = _ref1[0], offset = _ref1[1];
    if ((lastLine != null) && lastLine === firstLine) {
      lastLine = lastLine.next;
    }
    while ((firstLine != null) && firstLine !== lastLine) {
      formatsArr.push(_.clone(firstLine.formats));
      firstLine = firstLine.next;
    }
    return this._intersectFormats(formatsArr);
  };

  Toolbar.prototype._intersectFormats = function(formatsArr) {
    return _.reduce(formatsArr.slice(1), function(activeFormats, formats) {
      var activeKeys, added, formatKeys, intersection, missing;
      activeKeys = Object.keys(activeFormats);
      formatKeys = formats != null ? Object.keys(formats) : {};
      intersection = _.intersection(activeKeys, formatKeys);
      missing = _.difference(activeKeys, formatKeys);
      added = _.difference(formatKeys, activeKeys);
      _.each(intersection, function(name) {
        if (Toolbar.formats.SELECT[name] != null) {
          if (Array.isArray(activeFormats[name])) {
            if (activeFormats[name].indexOf(formats[name]) < 0) {
              return activeFormats[name].push(formats[name]);
            }
          } else if (activeFormats[name] !== formats[name]) {
            return activeFormats[name] = [activeFormats[name], formats[name]];
          }
        }
      });
      _.each(missing, function(name) {
        if (Toolbar.formats.TOGGLE[name] != null) {
          return delete activeFormats[name];
        } else if ((Toolbar.formats.SELECT[name] != null) && !Array.isArray(activeFormats[name])) {
          return activeFormats[name] = [activeFormats[name]];
        }
      });
      _.each(added, function(name) {
        if (Toolbar.formats.SELECT[name] != null) {
          return activeFormats[name] = [formats[name]];
        }
      });
      return activeFormats;
    }, formatsArr[0] || {});
  };

  return Toolbar;

})();

Quill.registerModule('toolbar', Toolbar);

module.exports = Toolbar;



},{"../quill":30}],28:[function(_dereq_,module,exports){
var Normalizer, Quill, Tooltip, dom, _;

Quill = _dereq_('../quill');

Normalizer = _dereq_('../lib/normalizer');

_ = Quill.require('lodash');

dom = Quill.require('dom');

Tooltip = (function() {
  Tooltip.DEFAULTS = {
    offset: 10,
    template: ''
  };

  Tooltip.HIDE_MARGIN = '-10000px';

  function Tooltip(quill, options) {
    this.quill = quill;
    this.options = options;
    this.container = this.quill.addContainer('ql-tooltip');
    this.container.innerHTML = Normalizer.stripWhitespace(this.options.template);
    dom(this.quill.root).on('focus', _.bind(this.hide, this));
    this.hide();
    this.quill.on(this.quill.constructor.events.TEXT_CHANGE, (function(_this) {
      return function(delta, source) {
        if (source === 'user' && _this.container.style.left !== Tooltip.HIDE_MARGIN) {
          _this.range = null;
          return _this.hide();
        }
      };
    })(this));
  }

  Tooltip.prototype.initTextbox = function(textbox, enterCallback, escapeCallback) {
    return dom(textbox).on('keyup', (function(_this) {
      return function(event) {
        switch (event.which) {
          case dom.KEYS.ENTER:
            return enterCallback.call(_this);
          case dom.KEYS.ESCAPE:
            return escapeCallback.call(_this);
          default:
            return true;
        }
      };
    })(this));
  };

  Tooltip.prototype.hide = function() {
    this.container.style.left = Tooltip.HIDE_MARGIN;
    if (this.range) {
      this.quill.setSelection(this.range);
    }
    return this.range = null;
  };

  Tooltip.prototype.position = function(reference) {
    var left, offsetBottom, offsetLeft, offsetTop, parentBounds, referenceBounds, top;
    if (reference != null) {
      referenceBounds = reference.getBoundingClientRect();
      parentBounds = this.quill.container.getBoundingClientRect();
      offsetLeft = referenceBounds.left - parentBounds.left;
      offsetTop = referenceBounds.top - parentBounds.top;
      offsetBottom = referenceBounds.bottom - parentBounds.bottom;
      left = offsetLeft + referenceBounds.width / 2 - this.container.offsetWidth / 2;
      top = offsetTop + referenceBounds.height + this.options.offset;
      if (top + this.container.offsetHeight > this.quill.container.offsetHeight) {
        top = offsetTop - this.container.offsetHeight - this.options.offset;
      }
      left = Math.max(0, Math.min(left, this.quill.container.offsetWidth - this.container.offsetWidth));
      top = Math.max(0, Math.min(top, this.quill.container.offsetHeight - this.container.offsetHeight));
    } else {
      left = this.quill.container.offsetWidth / 2 - this.container.offsetWidth / 2;
      top = this.quill.container.offsetHeight / 2 - this.container.offsetHeight / 2;
    }
    top += this.quill.container.scrollTop;
    return [left, top];
  };

  Tooltip.prototype.show = function(reference) {
    var left, top, _ref;
    this.range = this.quill.getSelection();
    _ref = this.position(reference), left = _ref[0], top = _ref[1];
    this.container.style.left = "" + left + "px";
    this.container.style.top = "" + top + "px";
    return this.container.focus();
  };

  return Tooltip;

})();

Quill.registerModule('tooltip', Tooltip);

module.exports = Tooltip;



},{"../lib/normalizer":18,"../quill":30}],29:[function(_dereq_,module,exports){
var Delta, Quill, UndoManager, _;

Quill = _dereq_('../quill');

_ = Quill.require('lodash');

Delta = Quill.require('delta');

UndoManager = (function() {
  UndoManager.DEFAULTS = {
    delay: 1000,
    maxStack: 100
  };

  UndoManager.hotkeys = {
    UNDO: {
      key: 'Z',
      metaKey: true
    },
    REDO: {
      key: 'Z',
      metaKey: true,
      shiftKey: true
    }
  };

  function UndoManager(quill, options) {
    this.quill = quill;
    this.options = options != null ? options : {};
    this.lastRecorded = 0;
    this.ignoreChange = false;
    this.clear();
    this.initListeners();
  }

  UndoManager.prototype.initListeners = function() {
    this.quill.onModuleLoad('keyboard', (function(_this) {
      return function(keyboard) {
        keyboard.addHotkey(UndoManager.hotkeys.UNDO, function() {
          _this.quill.editor.checkUpdate();
          _this.undo();
          return false;
        });
        return keyboard.addHotkey(UndoManager.hotkeys.REDO, function() {
          _this.quill.editor.checkUpdate();
          _this.redo();
          return false;
        });
      };
    })(this));
    return this.quill.on(this.quill.constructor.events.TEXT_CHANGE, (function(_this) {
      return function(delta, origin) {
        if (_this.ignoreChange) {
          return;
        }
        _this.record(delta, _this.oldDelta);
        return _this.oldDelta = _this.quill.getContents();
      };
    })(this));
  };

  UndoManager.prototype.clear = function() {
    this.stack = {
      undo: [],
      redo: []
    };
    return this.oldDelta = this.quill.getContents();
  };

  UndoManager.prototype.record = function(changeDelta, oldDelta) {
    var change, ignored, timestamp, undoDelta;
    if (!(changeDelta.ops.length > 0)) {
      return;
    }
    this.stack.redo = [];
    try {
      undoDelta = this.quill.getContents().diff(this.oldDelta);
      timestamp = new Date().getTime();
      if (this.lastRecorded + this.options.delay > timestamp && this.stack.undo.length > 0) {
        change = this.stack.undo.pop();
        undoDelta = undoDelta.compose(change.undo);
        changeDelta = change.redo.compose(changeDelta);
      } else {
        this.lastRecorded = timestamp;
      }
      this.stack.undo.push({
        redo: changeDelta,
        undo: undoDelta
      });
      if (this.stack.undo.length > this.options.maxStack) {
        return this.stack.undo.unshift();
      }
    } catch (_error) {
      ignored = _error;
      console.warn('Could not record change... clearing undo stack.');
      return this.clear();
    }
  };

  UndoManager.prototype.redo = function() {
    return this._change('redo', 'undo');
  };

  UndoManager.prototype.undo = function() {
    return this._change('undo', 'redo');
  };

  UndoManager.prototype._getLastChangeIndex = function(delta) {
    var index, lastIndex;
    lastIndex = 0;
    index = 0;
    _.each(delta.ops, function(op) {
      if (op.insert != null) {
        return lastIndex = Math.max(index + (op.insert.length || 1), lastIndex);
      } else if (op["delete"] != null) {
        return lastIndex = Math.max(index, lastIndex);
      } else if (op.retain != null) {
        if (op.attributes != null) {
          lastIndex = Math.max(index + op.retain, lastIndex);
        }
        return index += op.retain;
      }
    });
    return lastIndex;
  };

  UndoManager.prototype._change = function(source, dest) {
    var change, index;
    if (this.stack[source].length > 0) {
      change = this.stack[source].pop();
      this.lastRecorded = 0;
      this.ignoreChange = true;
      this.quill.updateContents(change[source], 'user');
      this.ignoreChange = false;
      index = this._getLastChangeIndex(change[source]);
      this.quill.setSelection(index, index);
      this.oldDelta = this.quill.getContents();
      return this.stack[dest].push(change);
    }
  };

  return UndoManager;

})();

Quill.registerModule('undo-manager', UndoManager);

module.exports = UndoManager;



},{"../quill":30}],30:[function(_dereq_,module,exports){
var Delta, Editor, EventEmitter2, Format, Leaf, Line, Normalizer, Quill, Range, dom, pkg, _,
  __hasProp = {}.hasOwnProperty,
  __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
  __slice = [].slice;

_ = _dereq_('lodash');

pkg = _dereq_('../package.json');

Delta = _dereq_('rich-text/lib/delta');

EventEmitter2 = _dereq_('eventemitter2').EventEmitter2;

dom = _dereq_('./lib/dom');

Editor = _dereq_('./core/editor');

Format = _dereq_('./core/format');

Range = _dereq_('./lib/range');

Normalizer = _dereq_('./lib/normalizer');

Line = _dereq_('./core/line');

Leaf = _dereq_('./core/leaf');

Quill = (function(_super) {
  __extends(Quill, _super);

  Quill.version = pkg.version;

  Quill.editors = [];

  Quill.modules = [];

  Quill.themes = [];

  Quill.DEFAULTS = {
    formats: ['align', 'bold', 'italic', 'strike', 'underline', 'color', 'background', 'font', 'size', 'link', 'image', 'bullet', 'list'],
    modules: {
      'keyboard': true,
      'paste-manager': true,
      'undo-manager': true
    },
    pollInterval: 100,
    readOnly: false,
    styles: {},
    theme: 'base'
  };

  Quill.events = {
    MODULE_INIT: 'module-init',
    POST_EVENT: 'post-event',
    PRE_EVENT: 'pre-event',
    SELECTION_CHANGE: 'selection-change',
    TEXT_CHANGE: 'text-change'
  };

  Quill.sources = Editor.sources;

  Quill.registerModule = function(name, module) {
    if (Quill.modules[name] != null) {
      console.warn("Overwriting " + name + " module");
    }
    return Quill.modules[name] = module;
  };

  Quill.registerTheme = function(name, theme) {
    if (Quill.themes[name] != null) {
      console.warn("Overwriting " + name + " theme");
    }
    return Quill.themes[name] = theme;
  };

  Quill.require = function(name) {
    switch (name) {
      case 'lodash':
        return _;
      case 'delta':
        return Delta;
      case 'dom':
        return dom;
      case 'normalizer':
        return Normalizer;
      case 'core/line':
        return Line;
      case 'core/leaf':
        return Leaf;
      case 'core/format':
        return Format;
      default:
        return null;
    }
  };

  function Quill(container, options) {
    var html, moduleOptions, themeClass;
    this.container = container;
    if (options == null) {
      options = {};
    }
    if (_.isString(this.container)) {
      this.container = document.querySelector(container);
    }
    if (this.container == null) {
      throw new Error('Invalid Quill container');
    }
    moduleOptions = _.defaults(options.modules || {}, Quill.DEFAULTS.modules);
    html = this.container.innerHTML;
    this.container.innerHTML = '';
    this.options = _.defaults(options, Quill.DEFAULTS);
    this.options.modules = moduleOptions;
    this.options.id = this.id = "ql-editor-" + (Quill.editors.length + 1);
    this.options.emitter = this;
    this.modules = {};
    this.root = this.addContainer('ql-editor');
    this.editor = new Editor(this.root, this, this.options);
    Quill.editors.push(this);
    this.setHTML(html, Quill.sources.SILENT);
    themeClass = Quill.themes[this.options.theme];
    if (themeClass == null) {
      throw new Error("Cannot load " + this.options.theme + " theme. Are you sure you registered it?");
    }
    this.theme = new themeClass(this, this.options);
    _.each(this.options.modules, (function(_this) {
      return function(option, name) {
        return _this.addModule(name, option);
      };
    })(this));
  }

  Quill.prototype.destroy = function() {
    var html;
    html = this.getHTML();
    _.each(this.modules, function(module, name) {
      if (_.isFunction(module.destroy)) {
        return module.destroy();
      }
    });
    this.editor.destroy();
    this.removeAllListeners();
    Quill.editors.splice(_.indexOf(Quill.editors, this), 1);
    return this.container.innerHTML = html;
  };

  Quill.prototype.addContainer = function(className, before) {
    var container, refNode;
    if (before == null) {
      before = false;
    }
    refNode = before ? this.root : null;
    container = document.createElement('div');
    dom(container).addClass(className);
    this.container.insertBefore(container, refNode);
    return container;
  };

  Quill.prototype.addFormat = function(name, format) {
    return this.editor.doc.addFormat(name, format);
  };

  Quill.prototype.addModule = function(name, options) {
    var moduleClass;
    moduleClass = Quill.modules[name];
    if (moduleClass == null) {
      throw new Error("Cannot load " + name + " module. Are you sure you registered it?");
    }
    if (options === true) {
      options = {};
    }
    options = _.defaults(options, this.theme.constructor.OPTIONS[name] || {}, moduleClass.DEFAULTS || {});
    this.modules[name] = new moduleClass(this, options);
    this.emit(Quill.events.MODULE_INIT, name, this.modules[name]);
    return this.modules[name];
  };

  Quill.prototype.deleteText = function(start, end, source) {
    var delta, formats, _ref;
    if (source == null) {
      source = Quill.sources.API;
    }
    _ref = this._buildParams(start, end, {}, source), start = _ref[0], end = _ref[1], formats = _ref[2], source = _ref[3];
    if (!(end > start)) {
      return;
    }
    delta = new Delta().retain(start)["delete"](end - start);
    return this.editor.applyDelta(delta, source);
  };

  Quill.prototype.emit = function() {
    var args, eventName;
    eventName = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
    Quill.__super__.emit.apply(this, [Quill.events.PRE_EVENT, eventName].concat(__slice.call(args)));
    Quill.__super__.emit.apply(this, [eventName].concat(__slice.call(args)));
    return Quill.__super__.emit.apply(this, [Quill.events.POST_EVENT, eventName].concat(__slice.call(args)));
  };

  Quill.prototype.focus = function() {
    return this.editor.focus();
  };

  Quill.prototype.formatLine = function(start, end, name, value, source) {
    var formats, line, offset, _ref, _ref1;
    _ref = this._buildParams(start, end, name, value, source), start = _ref[0], end = _ref[1], formats = _ref[2], source = _ref[3];
    _ref1 = this.editor.doc.findLineAt(end), line = _ref1[0], offset = _ref1[1];
    if (line != null) {
      end += line.length - offset;
    }
    return this.formatText(start, end, formats, source);
  };

  Quill.prototype.formatText = function(start, end, name, value, source) {
    var delta, formats, _ref;
    _ref = this._buildParams(start, end, name, value, source), start = _ref[0], end = _ref[1], formats = _ref[2], source = _ref[3];
    formats = _.reduce(formats, (function(_this) {
      return function(formats, value, name) {
        var format;
        format = _this.editor.doc.formats[name];
        if (!(value && value !== format.config["default"])) {
          formats[name] = null;
        }
        return formats;
      };
    })(this), formats);
    delta = new Delta().retain(start).retain(end - start, formats);
    return this.editor.applyDelta(delta, source);
  };

  Quill.prototype.getBounds = function(index) {
    return this.editor.getBounds(index);
  };

  Quill.prototype.getContents = function(start, end) {
    if (start == null) {
      start = 0;
    }
    if (end == null) {
      end = null;
    }
    if (_.isObject(start)) {
      end = start.end;
      start = start.start;
    }
    return this.editor.getDelta().slice(start, end);
  };

  Quill.prototype.getHTML = function() {
    return this.editor.doc.getHTML();
  };

  Quill.prototype.getLength = function() {
    return this.editor.getDelta().length();
  };

  Quill.prototype.getModule = function(name) {
    return this.modules[name];
  };

  Quill.prototype.getSelection = function() {
    this.editor.checkUpdate();
    return this.editor.selection.getRange();
  };

  Quill.prototype.getText = function(start, end) {
    if (start == null) {
      start = 0;
    }
    if (end == null) {
      end = null;
    }
    return _.map(this.getContents(start, end).ops, function(op) {
      if (_.isString(op.insert)) {
        return op.insert;
      } else {
        return '';
      }
    }).join('');
  };

  Quill.prototype.insertEmbed = function(index, type, url, source) {
    return this.insertText(index, dom.EMBED_TEXT, type, url, source);
  };

  Quill.prototype.insertText = function(index, text, name, value, source) {
    var delta, end, formats, _ref;
    _ref = this._buildParams(index, 0, name, value, source), index = _ref[0], end = _ref[1], formats = _ref[2], source = _ref[3];
    if (!(text.length > 0)) {
      return;
    }
    delta = new Delta().retain(index).insert(text, formats);
    return this.editor.applyDelta(delta, source);
  };

  Quill.prototype.onModuleLoad = function(name, callback) {
    if (this.modules[name]) {
      return callback(this.modules[name]);
    }
    return this.on(Quill.events.MODULE_INIT, function(moduleName, module) {
      if (moduleName === name) {
        return callback(module);
      }
    });
  };

  Quill.prototype.prepareFormat = function(name, value) {
    var format, range;
    format = this.editor.doc.formats[name];
    if (format == null) {
      return;
    }
    range = this.getSelection();
    if (!(range != null ? range.isCollapsed() : void 0)) {
      return;
    }
    if (format.isType(Format.types.LINE)) {
      return this.formatLine(range, name, value, Quill.sources.USER);
    } else {
      return format.prepare(value);
    }
  };

  Quill.prototype.setContents = function(delta, source) {
    if (source == null) {
      source = Quill.sources.API;
    }
    if (Array.isArray(delta)) {
      delta = {
        ops: delta.slice()
      };
    } else {
      delta = {
        ops: delta.ops.slice()
      };
    }
    delta.ops.push({
      "delete": this.getLength()
    });
    return this.updateContents(delta, source);
  };

  Quill.prototype.setHTML = function(html, source) {
    if (source == null) {
      source = Quill.sources.API;
    }
    if (!html.trim()) {
      html = "<" + dom.DEFAULT_BLOCK_TAG + "><" + dom.DEFAULT_BREAK_TAG + "></" + dom.DEFAULT_BLOCK_TAG + ">";
    }
    this.editor.doc.setHTML(html);
    return this.editor.checkUpdate(source);
  };

  Quill.prototype.setSelection = function(start, end, source) {
    var range;
    if (source == null) {
      source = Quill.sources.API;
    }
    if (_.isNumber(start) && _.isNumber(end)) {
      range = new Range(start, end);
    } else {
      range = start;
      source = end || source;
    }
    return this.editor.selection.setRange(range, source);
  };

  Quill.prototype.setText = function(text, source) {
    var delta;
    if (source == null) {
      source = Quill.sources.API;
    }
    delta = new Delta().insert(text);
    return this.setContents(delta, source);
  };

  Quill.prototype.updateContents = function(delta, source) {
    if (source == null) {
      source = Quill.sources.API;
    }
    return this.editor.applyDelta(delta, source);
  };

  Quill.prototype._buildParams = function() {
    var formats, params;
    params = 1 <= arguments.length ? __slice.call(arguments, 0) : [];
    if (_.isObject(params[0])) {
      params.splice(0, 1, params[0].start, params[0].end);
    }
    if (_.isString(params[2])) {
      formats = {};
      formats[params[2]] = params[3];
      params.splice(2, 2, formats);
    }
    if (params[3] == null) {
      params[3] = Quill.sources.API;
    }
    return params;
  };

  return Quill;

})(EventEmitter2);

Quill.registerTheme('base', _dereq_('./themes/base'));

Quill.registerTheme('snow', _dereq_('./themes/snow'));

module.exports = Quill;



},{"../package.json":7,"./core/editor":9,"./core/format":10,"./core/leaf":11,"./core/line":12,"./lib/dom":16,"./lib/normalizer":18,"./lib/range":20,"./themes/base":32,"./themes/snow":33,"eventemitter2":2,"lodash":1,"rich-text/lib/delta":3}],31:[function(_dereq_,module,exports){
module.exports = ".ql-image-tooltip{padding:10px;width:300px}.ql-image-tooltip:after{clear:both;content:\"\";display:table}.ql-image-tooltip a{border:1px solid #000;box-sizing:border-box;display:inline-block;float:left;padding:5px;text-align:center;width:50%}.ql-image-tooltip img{bottom:0;left:0;margin:auto;max-height:100%;max-width:100%;position:absolute;right:0;top:0}.ql-image-tooltip .input{box-sizing:border-box;width:100%}.ql-image-tooltip .preview{margin:10px 0;position:relative;border:1px dashed #000;height:200px}.ql-image-tooltip .preview span{display:inline-block;position:absolute;text-align:center;top:40%;width:100%}.ql-link-tooltip{padding:5px 10px}.ql-link-tooltip input.input{width:170px}.ql-link-tooltip a.done,.ql-link-tooltip input.input{display:none}.ql-link-tooltip.editing a.done,.ql-link-tooltip.editing input.input{display:inline-block}.ql-link-tooltip.editing a.change,.ql-link-tooltip.editing a.url{display:none}.ql-multi-cursor{position:absolute;left:0;top:0;z-index:1000}.ql-multi-cursor .cursor{margin-left:-1px;position:absolute}.ql-multi-cursor .cursor-flag{bottom:100%;position:absolute;white-space:nowrap}.ql-multi-cursor .cursor-name{display:inline-block;color:#fff;padding:2px 8px}.ql-multi-cursor .cursor-caret{height:100%;position:absolute;width:2px}.ql-multi-cursor .cursor.hidden .cursor-flag{display:none}.ql-multi-cursor .cursor.top .cursor-flag{bottom:auto;top:100%}.ql-multi-cursor .cursor.right .cursor-flag{right:-2px}.ql-paste-manager{left:-100000px;position:absolute;top:50%}.ql-toolbar{box-sizing:border-box}.ql-tooltip{background-color:#fff;border:1px solid #000;box-sizing:border-box;position:absolute;top:0;white-space:nowrap;z-index:2000}.ql-tooltip a{cursor:pointer;text-decoration:none}.ql-container{box-sizing:border-box;cursor:text;font-family:Helvetica,Arial,sans-serif;font-size:13px;height:100%;line-height:1.42;margin:0;overflow-x:hidden;overflow-y:auto;padding:12px 15px;position:relative}.ql-editor{box-sizing:border-box;min-height:100%;outline:0;tab-size:4;white-space:pre-wrap}.ql-editor div{margin:0;padding:0}.ql-editor a{text-decoration:underline}.ql-editor b{font-weight:700}.ql-editor i{font-style:italic}.ql-editor s{text-decoration:line-through}.ql-editor u{text-decoration:underline}.ql-editor img{max-width:100%}.ql-editor blockquote,.ql-editor ol,.ql-editor ul{margin:0 0 0 2em;padding:0}.ql-editor ol{list-style-type:decimal}.ql-editor ul{list-style-type:disc}.ql-editor.ql-ie-10 br,.ql-editor.ql-ie-9 br{display:none}";
},{}],32:[function(_dereq_,module,exports){
var BaseTheme, baseStyles, dom, _;

_ = _dereq_('lodash');

dom = _dereq_('../../lib/dom');

baseStyles = _dereq_('./base.styl');

BaseTheme = (function() {
  BaseTheme.OPTIONS = {};

  BaseTheme.objToCss = function(obj) {
    return _.map(obj, function(value, key) {
      var innerStr;
      innerStr = _.map(value, function(innerValue, innerKey) {
        return "" + innerKey + ": " + innerValue + ";";
      }).join(' ');
      return "" + key + " { " + innerStr + " }";
    }).join("\n");
  };

  function BaseTheme(quill, options) {
    var version;
    this.quill = quill;
    this.options = options;
    dom(this.quill.container).addClass('ql-container');
    if (this.options.styles) {
      this.addStyles(baseStyles + BaseTheme.objToCss(this.options.styles));
    }
    if (dom.isIE(10)) {
      version = dom.isIE(9) ? '9' : '10';
      dom(this.quill.root).addClass('ql-ie-' + version);
    }
  }

  BaseTheme.prototype.addStyles = function(css) {
    var style;
    if (_.isObject(css)) {
      css = BaseTheme.objToCss(css);
    }
    style = document.createElement('style');
    style.type = 'text/css';
    style.appendChild(document.createTextNode(css));
    return document.head.appendChild(style);
  };

  return BaseTheme;

})();

module.exports = BaseTheme;



},{"../../lib/dom":16,"./base.styl":31,"lodash":1}],33:[function(_dereq_,module,exports){
var BaseTheme, ColorPicker, Picker, SnowTheme, dom, _,
  __hasProp = {}.hasOwnProperty,
  __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

_ = _dereq_('lodash');

ColorPicker = _dereq_('../../lib/color-picker');

BaseTheme = _dereq_('../base');

dom = _dereq_('../../lib/dom');

Picker = _dereq_('../../lib/picker');

SnowTheme = (function(_super) {
  __extends(SnowTheme, _super);

  SnowTheme.COLORS = ["#000000", "#e60000", "#ff9900", "#ffff00", "#008A00", "#0066cc", "#9933ff", "#ffffff", "#facccc", "#ffebcc", "#ffffcc", "#cce8cc", "#cce0f5", "#ebd6ff", "#bbbbbb", "#f06666", "#ffc266", "#ffff66", "#66b966", "#66a3e0", "#c285ff", "#888888", "#a10000", "#b26b00", "#b2b200", "#006100", "#0047b2", "#6b24b2", "#444444", "#5c0000", "#663d00", "#666600", "#003700", "#002966", "#3d1466"];

  SnowTheme.OPTIONS = {
    'multi-cursor': {
      template: '<span class="cursor-flag"> <span class="cursor-triangle top"></span> <span class="cursor-name"></span> <span class="cursor-triangle bottom"></span> </span> <span class="cursor-caret"></span>'
    }
  };

  function SnowTheme(quill, options) {
    this.quill = quill;
    this.options = options;
    SnowTheme.__super__.constructor.apply(this, arguments);
    dom(this.quill.container).addClass('ql-snow');
    this.pickers = [];
    this.quill.on(this.quill.constructor.events.SELECTION_CHANGE, (function(_this) {
      return function(range) {
        if (range != null) {
          return _.invoke(_this.pickers, 'close');
        }
      };
    })(this));
    this.quill.onModuleLoad('multi-cursor', _.bind(this.extendMultiCursor, this));
    this.quill.onModuleLoad('toolbar', _.bind(this.extendToolbar, this));
  }

  SnowTheme.prototype.extendMultiCursor = function(module) {
    return module.on(module.constructor.events.CURSOR_ADDED, function(cursor) {
      var bottomTriangle, topTriangle;
      bottomTriangle = cursor.elem.querySelector('.cursor-triangle.bottom');
      topTriangle = cursor.elem.querySelector('.cursor-triangle.top');
      return bottomTriangle.style.borderTopColor = topTriangle.style.borderBottomColor = cursor.color;
    });
  };

  SnowTheme.prototype.extendToolbar = function(module) {
    dom(module.container).addClass('ql-snow');
    _.each(['color', 'background', 'font', 'size', 'align'], (function(_this) {
      return function(format) {
        var picker, select;
        select = module.container.querySelector(".ql-" + format);
        if (select == null) {
          return;
        }
        switch (format) {
          case 'font':
          case 'size':
          case 'align':
            picker = new Picker(select);
            break;
          case 'color':
          case 'background':
            picker = new ColorPicker(select);
            _.each(picker.container.querySelectorAll('.ql-picker-item'), function(item, i) {
              if (i < 7) {
                return dom(item).addClass('ql-primary-color');
              }
            });
        }
        if (picker != null) {
          return _this.pickers.push(picker);
        }
      };
    })(this));
    return _.each(dom(module.container).textNodes(), function(node) {
      if (dom(node).text().trim().length === 0) {
        return dom(node).remove();
      }
    });
  };

  return SnowTheme;

})(BaseTheme);

module.exports = SnowTheme;



},{"../../lib/color-picker":15,"../../lib/dom":16,"../../lib/picker":19,"../base":32,"lodash":1}]},{},[14])(14)
});
}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],18:[function(require,module,exports){
module.exports = require('./dist/quill');

},{"./dist/quill":17}],19:[function(require,module,exports){
var MathTooltip, Quill, Tooltip, dom, katex, renderAllMath, _,
  __hasProp = {}.hasOwnProperty,
  __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

Quill = require('quill');

Tooltip = Quill.modules.tooltip;

_ = Quill.require('lodash');

dom = Quill.require('dom');

katex = require('katex');

MathTooltip = (function(_super) {
  __extends(MathTooltip, _super);

  MathTooltip.DEFAULTS = {
    maxLength: 50,
    template: '<span class="title">Edit Formula &nbsp;</span> <input class="math-input" type="text"> <span>&nbsp;&#45;&nbsp;</span> <div class="preview"></div> <div class="preview-error"></div> <button class="cancel">Cancel</button> <button class="update">Update</button> <button class="remove">Remove</button>'
  };

  function MathTooltip(quill, options) {
    this.quill = quill;
    this.options = options;
    this.options = _.defaults(this.options, Tooltip.DEFAULTS);
    MathTooltip.__super__.constructor.call(this, this.quill, this.options);
    dom(this.container).addClass('ql-math-tooltip');
    this.textbox = this.container.querySelector('.math-input');
    this.preview = this.container.querySelector('.preview');
    this.previewError = this.container.querySelector('.preview-error');
    this.updateButton = this.container.querySelector('.update');
    this.removeButton = this.container.querySelector('.remove');
    this.initListeners();
  }

  MathTooltip.prototype.initListeners = function() {
    this.quill.on(this.quill.constructor.events.SELECTION_CHANGE, (function(_this) {
      return function(range) {
        var anchor, formula;
        renderAllMath(_this.quill);
        if (!((range != null) && range.isCollapsed())) {
          return;
        }
        anchor = _this._findAnchor(range);
        if (anchor) {
          formula = dom(anchor).attributes()['data-math'].substring('math:'.length);
          _this.setMode(formula, true);
          _this._currentInitialFormula = formula;
          _this._currentMathEl = anchor;
          return _this.show(anchor);
        } else {
          _this.range = null;
          _this.range = null;
          return _this.hide();
        }
      };
    })(this));
    dom(this.updateButton).on('click', _.bind(this.saveMath, this));
    dom(this.removeButton).on('click', _.bind(this.removeMath, this));
    dom(this.container.querySelector('.cancel')).on('click', _.bind(this.hide, this));
    this.range = null;
    this.initTextbox(this.textbox, this.saveMath, this.hide);
    this._updateMathPreview();
    this.quill.onModuleLoad('toolbar', (function(_this) {
      return function(toolbar) {
        return toolbar.initFormat('math', _.bind(_this._onToolbar, _this));
      };
    })(this));
    dom(this.textbox).on('keyup', _.bind(this._updateMathPreview, this));
    return renderAllMath(this.quill);
  };

  MathTooltip.prototype._updateMathPreview = function() {
    var e, formula;
    formula = this.textbox.value;
    if (formula !== this._currentInitialFormula) {
      try {
        katex.render(formula, this.preview);
        dom(this.updateButton).removeClass('disabled');
        return this.previewError.innerHTML = '';
      } catch (_error) {
        e = _error;
        dom(this.updateButton).addClass('disabled');
        this.preview.innerHTML = '';
        return this.previewError.innerHTML = "Parse Problem: " + e.message;
      }
    } else {
      dom(this.updateButton).addClass('disabled');
      this.preview.innerHTML = '';
      return this.previewError.innerHTML = '';
    }
  };

  MathTooltip.prototype.renderMath = function(node) {
    var e, formula;
    formula = dom(node).attributes()['data-math'].substring('math:'.length);
    try {
      return katex.render(formula, node);
    } catch (_error) {
      e = _error;
      return node.innerHTML = e.message;
    }
  };

  MathTooltip.prototype.show = function() {
    this._updateMathPreview();
    return MathTooltip.__super__.show.apply(this, arguments);
  };

  MathTooltip.prototype.hide = function() {
    this.range = null;
    this._currentInitialFormula = null;
    return MathTooltip.__super__.hide.apply(this, arguments);
  };

  MathTooltip.prototype.saveMath = function() {
    var anchor, e, formula, math, url, _i, _len, _ref;
    url = this.textbox.value;
    if (this.range != null) {
      if (this.range.isCollapsed()) {
        anchor = this._findAnchor(this.range);
        if (anchor != null) {
          dom(anchor).attributes({
            'data-math': "math:" + url
          });
        }
        this.renderMath(anchor);
      } else {
        this.quill.formatText(this.range, 'math', "math:" + url, 'user');
        _ref = this.quill.editor.root.querySelectorAll('[data-math^="math:"]');
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
          math = _ref[_i];
          formula = dom(math).attributes()['data-math'].substring('math:'.length);
          try {
            katex.render(formula, math);
          } catch (_error) {
            e = _error;
            console.log('Error: Invalid math');
          }
        }
      }
    }
    this.setMode(url, false);
    return this.hide();
  };

  MathTooltip.prototype.removeMath = function() {
    var formula, node, _ref;
    node = this._findAnchor(this.range);
    formula = (_ref = dom(node).attributes()['data-math']) != null ? _ref.substring('math:'.length) : void 0;
    node.removeAttribute('data-math');
    node = dom(node).switchTag(dom.DEFAULT_INLINE_TAG);
    node.removeClass('loaded');
    return node.text(formula);
  };

  MathTooltip.prototype.setMode = function(url, edit) {
    if (edit == null) {
      edit = false;
    }
    if (edit) {
      this.textbox.value = url;
      _.defer((function(_this) {
        return function() {
          _this.textbox.focus();
          return _this.textbox.setSelectionRange(url.length, url.length);
        };
      })(this));
    } else {
      this.textbox.value = url;
    }
    return dom(this.container).toggleClass('editing', edit);
  };

  MathTooltip.prototype._findAnchor = function(range) {
    var leaf, node, offset, _ref, _ref1;
    _ref = this.quill.editor.doc.findLeafAt(range.start, true), leaf = _ref[0], offset = _ref[1];
    if (leaf != null) {
      node = leaf.node;
    }
    while (node != null) {
      if ((_ref1 = dom(node).attributes()['data-math']) != null ? _ref1.substring('math:'.length) : void 0) {
        return node;
      }
      node = node.parentNode;
    }
    return null;
  };

  MathTooltip.prototype._onToolbar = function(range, value) {
    var nativeRange;
    if (!(range && !range.isCollapsed())) {
      return;
    }
    if (value) {
      this.setMode(this._suggestURL(range), true);
      nativeRange = this.quill.editor.selection._getNativeRange();
      return this.show(nativeRange);
    }
  };

  MathTooltip.prototype._suggestURL = function(range) {
    var text;
    text = this.quill.getText(range);
    return text;
  };

  return MathTooltip;

})(Tooltip);

renderAllMath = function(quill) {
  var e, formula, math, _i, _len, _ref, _results;
  _ref = quill.editor.root.querySelectorAll('[data-math^="math:"]:not(.loaded)');
  _results = [];
  for (_i = 0, _len = _ref.length; _i < _len; _i++) {
    math = _ref[_i];
    formula = dom(math).attributes()['data-math'].substring('math:'.length);
    try {
      katex.render(formula, math);
      _results.push(math.classList.add('loaded'));
    } catch (_error) {
      e = _error;
      _results.push(console.log('Error: Invalid math'));
    }
  }
  return _results;
};

Quill.registerModule('math-tooltip', MathTooltip);

module.exports = MathTooltip;



},{"katex":2,"quill":18}]},{},[1]);

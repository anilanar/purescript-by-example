// Generated by psc-make version 0.6.9.3

/**
 *  | A type and functions for single characters.
 */
"use strict";
var Prelude = require("Prelude");

    function toCharCode(c) {
      return c.charCodeAt(0);
    }
    ;

    function fromCharCode(c) {
      return String.fromCharCode(c);
    }
    ;

/**
 * | A unicode character.
 */
var Char = function (x) {
    return x;
};

/**
 *  | Characters can be rendered as a string with `show`.
 */
var showChar = new Prelude.Show(function (_40) {
    return "Char " + Prelude.show(Prelude.showString)(_40);
});

/**
 *  | Characters can be compared for equality with `==` and `/=`.
 */
var eqChar = new Prelude.Eq(function (a) {
    return function (b) {
        return !Prelude["=="](eqChar)(a)(b);
    };
}, function (_36) {
    return function (_37) {
        return _36 === _37;
    };
});

/**
 *  | Characters can be compared with `compare`, `>`, `>=`, `<` and `<=`.
 */
var ordChar = new Prelude.Ord(function () {
    return eqChar;
}, function (_38) {
    return function (_39) {
        return Prelude.compare(Prelude.ordString)(_38)(_39);
    };
});

/**
 *  | Returns the string of length `1` containing only the given character.
 */
var charString = function (_35) {
    return _35;
};
module.exports = {
    toCharCode: toCharCode, 
    fromCharCode: fromCharCode, 
    charString: charString, 
    eqChar: eqChar, 
    ordChar: ordChar, 
    showChar: showChar
};

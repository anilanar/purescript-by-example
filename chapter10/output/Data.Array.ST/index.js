// Generated by psc-make version 0.6.9.3

/**
 *  | Helper functions for working with mutable arrays using the `ST` effect.
 *  |
 *  | This module can be used when performance is important and mutation is a local effect.
 */
"use strict";
var Data_Function = require("Data.Function");
var Prelude = require("Prelude");
var Data_Maybe = require("Data.Maybe");
var Control_Monad_Eff = require("Control.Monad.Eff");
var Control_Monad_ST = require("Control.Monad.ST");

  function runSTArray(f) {
    return f;
  };

  function emptySTArray() {
    return [];
  };

  function peekSTArrayImpl(just, nothing, arr, i) {
    return function() {
      var index = i >>> 0;
      return index < arr.length? just(arr[index]) : nothing;
    };
  };

  function pokeSTArrayImpl(arr, i, a) {
    return function() {
      var index = i >>> 0;
      var ret = index < arr.length;
      if (ret)
        arr[index] = a;
      return ret;
    };
  };

  function pushAllSTArrayImpl(arr, as) {
    return function(){
      return arr.push.apply(arr, as);
    };
  };

  function spliceSTArrayImpl(arr, index, howMany, bs) {
    return function(){
      return arr.splice.apply(arr, [index, howMany].concat(bs));
    };
  };

  function copyImpl(arr) {
    return function(){
      return arr.slice();
    };
  };

  function toAssocArray(arr) {
    return function(){
      var n = arr.length;
      var as = new Array(n);
      for (var i = 0; i < n; i++)
        as[i] = {value: arr[i], index: i};
      return as;
    };
  };

/**
 *  | Create a mutable copy of an immutable array.
 */
var thaw = copyImpl;

/**
 *  | Remove and/or insert elements from/into a mutable array at the specified index.
 */
var spliceSTArray = Data_Function.runFn4(spliceSTArrayImpl);

/**
 *  | Append the values in an immutable array to the end of a mutable array.
 */
var pushAllSTArray = Data_Function.runFn2(pushAllSTArrayImpl);

/**
 *  | Append an element to the end of a mutable array.
 */
var pushSTArray = function (arr) {
    return function (a) {
        return pushAllSTArray(arr)([ a ]);
    };
};

/**
 *  | Change the value at the specified index in a mutable array.
 */
var pokeSTArray = Data_Function.runFn3(pokeSTArrayImpl);

/**
 *  | Read the value at the specified index in a mutable array.
 */
var peekSTArray = Data_Function.runFn4(peekSTArrayImpl)(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);

/**
 *  | Create an immutable copy of a mutable array.
 */
var freeze = copyImpl;
module.exports = {
    toAssocArray: toAssocArray, 
    thaw: thaw, 
    freeze: freeze, 
    spliceSTArray: spliceSTArray, 
    pushAllSTArray: pushAllSTArray, 
    pushSTArray: pushSTArray, 
    pokeSTArray: pokeSTArray, 
    peekSTArray: peekSTArray, 
    emptySTArray: emptySTArray, 
    runSTArray: runSTArray
};

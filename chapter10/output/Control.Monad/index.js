// Generated by psc-make version 0.6.9.3

/**
 *  | This module defines helper functions for working with `Monad` instances.
 */
"use strict";
var Prelude = require("Prelude");

/**
 *  | Perform a monadic action when a condition is true.
 */
var when = function (__dict_Monad_0) {
    return function (_6) {
        return function (m) {
            if (_6) {
                return m;
            };
            if (!_6) {
                return Prelude["return"](__dict_Monad_0)(Prelude.unit);
            };
            throw new Error("Failed pattern match");
        };
    };
};

/**
 *  | Perform a monadic action unless a condition is true.
 */
var unless = function (__dict_Monad_1) {
    return function (_7) {
        return function (m) {
            if (!_7) {
                return m;
            };
            if (_7) {
                return Prelude["return"](__dict_Monad_1)(Prelude.unit);
            };
            throw new Error("Failed pattern match");
        };
    };
};

/**
 *  | Perform a monadic action `n` times collecting all of the results.
 */
var replicateM = function (__dict_Monad_2) {
    return function (n) {
        return function (m) {
            if (n === 0) {
                return Prelude["return"](__dict_Monad_2)([  ]);
            };
            return Prelude[">>="](__dict_Monad_2["__superclass_Prelude.Bind_1"]())(m)(function (_1) {
                return Prelude[">>="](__dict_Monad_2["__superclass_Prelude.Bind_1"]())(replicateM(__dict_Monad_2)(n - 1)(m))(function (_0) {
                    return Prelude["return"](__dict_Monad_2)(Prelude[":"](_1)(_0));
                });
            });
        };
    };
};

/**
 *  | Perform a fold using a monadic step function.
 */
var foldM = function (__dict_Monad_3) {
    return function (f) {
        return function (a) {
            return function (_5) {
                if (_5.length === 0) {
                    return Prelude["return"](__dict_Monad_3)(a);
                };
                if (_5.length >= 1) {
                    var _238 = _5.slice(1);
                    return Prelude[">>="](__dict_Monad_3["__superclass_Prelude.Bind_1"]())(f(a)(_5[0]))(function (a$prime) {
                        return foldM(__dict_Monad_3)(f)(a$prime)(_238);
                    });
                };
                throw new Error("Failed pattern match");
            };
        };
    };
};

/**
 *  | Filter where the predicate returns a monadic `Boolean`.
 *  |
 *  | For example: 
 *  |
 *  | ```purescript
 *  | powerSet :: forall a. [a] -> [[a]]
 *  | powerSet = filterM (const [true, false])
 *  | ```
 */
var filterM = function (__dict_Monad_4) {
    return function (p) {
        return function (_8) {
            if (_8.length === 0) {
                return Prelude["return"](__dict_Monad_4)([  ]);
            };
            if (_8.length >= 1) {
                var _245 = _8.slice(1);
                return Prelude[">>="](__dict_Monad_4["__superclass_Prelude.Bind_1"]())(p(_8[0]))(function (_3) {
                    return Prelude[">>="](__dict_Monad_4["__superclass_Prelude.Bind_1"]())(filterM(__dict_Monad_4)(p)(_245))(function (_2) {
                        return Prelude["return"](__dict_Monad_4)((function () {
                            if (_3) {
                                return Prelude[":"](_8[0])(_2);
                            };
                            if (!_3) {
                                return _2;
                            };
                            throw new Error("Failed pattern match");
                        })());
                    });
                });
            };
            throw new Error("Failed pattern match");
        };
    };
};
module.exports = {
    filterM: filterM, 
    unless: unless, 
    when: when, 
    foldM: foldM, 
    replicateM: replicateM
};

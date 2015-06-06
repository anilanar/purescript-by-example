// Generated by psc-make version 0.6.9.3
"use strict";
var Prelude = require("Prelude");
var Control_Alt = require("Control.Alt");
var Control_Alternative = require("Control.Alternative");
var Control_Extend = require("Control.Extend");
var Control_MonadPlus = require("Control.MonadPlus");
var Control_Plus = require("Control.Plus");

/**
 *  | The `Maybe` type is used to represent optional values and can be seen as
 *  | something like a type-safe `null`, where `Nothing` is `null` and `Just x`
 *  | is the non-null value `x`.
 */
var Nothing = (function () {
    function Nothing() {

    };
    Nothing.value = new Nothing();
    return Nothing;
})();

/**
 *  | The `Maybe` type is used to represent optional values and can be seen as
 *  | something like a type-safe `null`, where `Nothing` is `null` and `Just x`
 *  | is the non-null value `x`.
 */
var Just = (function () {
    function Just(value0) {
        this.value0 = value0;
    };
    Just.create = function (value0) {
        return new Just(value0);
    };
    return Just;
})();

/**
 *  | The `Show` instance allows `Maybe` values to be rendered as a string with
 *  | `show` whenever there is an `Show` instance for the type the `Maybe`
 *  | contains.
 */
var showMaybe = function (__dict_Show_0) {
    return new Prelude.Show(function (_29) {
        if (_29 instanceof Just) {
            return "Just (" + (Prelude.show(__dict_Show_0)(_29.value0) + ")");
        };
        if (_29 instanceof Nothing) {
            return "Nothing";
        };
        throw new Error("Failed pattern match");
    });
};

/**
 *  | The `Semigroup` instance enables use of the operator `<>` on `Maybe` values
 *  | whenever there is a `Semigroup` instance for the type the `Maybe` contains.
 *  | The exact behaviour of `<>` depends on the "inner" `Semigroup` instance,
 *  | but generally captures the notion of appending or combining things.
 *  |
 *  | ``` purescript
 *  | Just x <> Just y = Just (x <> y)
 *  | Just x <> Nothing = Just x
 *  | Nothing <> Just y = Just y
 *  | Nothing <> Nothing = Nothing
 *  | ```
 */
var semigroupMaybe = function (__dict_Semigroup_1) {
    return new Prelude.Semigroup(function (x) {
        return function (x_1) {
            if (x_1 instanceof Nothing) {
                return x_1;
            };
            if (x_1 instanceof Nothing) {
                return x_1;
            };
            if (x_1 instanceof Just && x_1 instanceof Just) {
                return new Just(Prelude["<>"](__dict_Semigroup_1)(x_1.value0)(x_1.value0));
            };
            throw new Error("Failed pattern match");
        };
    });
};

/**
 *  | Takes a default value, a function, and a `Maybe` value. If the `Maybe`
 *  | value is `Nothing` the default value is returned, otherwise the function
 *  | is applied to the value inside the `Just` and the result is returned.
 *  |
 *  | ``` purescript
 *  | maybe x f Nothing == x
 *  | maybe x f (Just y) == f y
 *  | ```
 */
var maybe = function (b) {
    return function (f) {
        return function (_25) {
            if (_25 instanceof Nothing) {
                return b;
            };
            if (_25 instanceof Just) {
                return f(_25.value0);
            };
            throw new Error("Failed pattern match");
        };
    };
};

/**
 *  | Returns `true` when the `Maybe` value is `Nothing`.
 */
var isNothing = maybe(true)(Prelude["const"](false));

/**
 *  | Returns `true` when the `Maybe` value was constructed with `Just`.
 */
var isJust = maybe(false)(Prelude["const"](true));

/**
 *  | The `Functor` instance allows functions to transform the contents of a
 *  | `Just` with the `<$>` operator:
 *  |
 *  | ``` purescript
 *  | f <$> Just x == Just (f x)
 *  | ```
 *  |
 *  | `Nothing` values are left untouched:
 *  |
 *  | ``` purescript
 *  | f <$> Nothing == Nothing
 *  | ```
 */
var functorMaybe = new Prelude.Functor(function (fn) {
    return function (_26) {
        if (_26 instanceof Just) {
            return new Just(fn(_26.value0));
        };
        return Nothing.value;
    };
});

/**
 *  | Takes a default value, and a `Maybe` value. If the `Maybe` value is
 *  | `Nothing` the default value is returned, otherwise the value inside the
 *  | `Just` is returned.
 *  |
 *  | ``` purescript
 *  | fromMaybe x Nothing == x
 *  | fromMaybe x (Just y) == y
 *  | ```
 */
var fromMaybe = function (a) {
    return maybe(a)(Prelude.id(Prelude.categoryArr));
};

/**
 *  | The `Extend` instance allows sequencing of `Maybe` values and functions
 *  | that accept a `Maybe a` and return a non-`Maybe` result using the
 *  | `<<=` operator.
 *  |
 *  | ``` purescript
 *  | f <<= Nothing = Nothing
 *  | f <<= Just x = Just (f x)
 *  | ```
 */
var extendMaybe = new Control_Extend.Extend(function (f) {
    return function (x) {
        if (x instanceof Nothing) {
            return Nothing.value;
        };
        return new Just(f(x));
    };
}, function () {
    return functorMaybe;
});

/**
 *  | The `Eq` instance allows `Maybe` values to be checked for equality with
 *  | `==` and inequality with `/=` whenever there is an `Eq` instance for the
 *  | type the `Maybe` contains.
 */
var eqMaybe = function (__dict_Eq_3) {
    return new Prelude.Eq(function (a) {
        return function (b) {
            return !Prelude["=="](eqMaybe(__dict_Eq_3))(a)(b);
        };
    }, function (_30) {
        return function (_31) {
            if (_30 instanceof Nothing && _31 instanceof Nothing) {
                return true;
            };
            if (_30 instanceof Just && _31 instanceof Just) {
                return Prelude["=="](__dict_Eq_3)(_30.value0)(_31.value0);
            };
            return false;
        };
    });
};

/**
 *  | The `Ord` instance allows `Maybe` values to be compared with
 *  | `compare`, `>`, `>=`, `<` and `<=` whenever there is an `Ord` instance for
 *  | the type the `Maybe` contains.
 *  |
 *  | `Nothing` is considered to be less than any `Just` value.
 */
var ordMaybe = function (__dict_Ord_2) {
    return new Prelude.Ord(function () {
        return eqMaybe(__dict_Ord_2["__superclass_Prelude.Eq_0"]());
    }, function (_32) {
        return function (_33) {
            if (_32 instanceof Just && _33 instanceof Just) {
                return Prelude.compare(__dict_Ord_2)(_32.value0)(_33.value0);
            };
            if (_32 instanceof Nothing && _33 instanceof Nothing) {
                return Prelude.EQ.value;
            };
            if (_32 instanceof Nothing) {
                return Prelude.LT.value;
            };
            if (_33 instanceof Nothing) {
                return Prelude.GT.value;
            };
            throw new Error("Failed pattern match");
        };
    });
};

/**
 *  | The `Apply` instance allows functions contained within a `Just` to
 *  | transform a value contained within a `Just` using the `(<*>)` operator:
 *  |
 *  | ``` purescript
 *  | Just f <*> Just x == Just (f x)
 *  | ```
 *  |
 *  | `Nothing` values are left untouched:
 *  |
 *  | ``` purescript
 *  | Just f <*> Nothing == Nothing
 *  | Nothing <*> Just x == Nothing
 *  | ```
 *  |
 *  | Combining `Functor`'s `<$>` with `Apply`'s `<*>` can be used transform a
 *  | pure function to take `Maybe`-typed arguments so `f :: a -> b -> c`
 *  | becomes `f :: Maybe a -> Maybe b -> Maybe c`:
 *  |
 *  | ``` purescript
 *  | f <$> Just x <*> Just y == Just (f x y)
 *  | ```
 *  |
 *  | The `Nothing`-preserving behaviour of both operators means the result of
 *  | an expression like the above but where any one of the values is `Nothing`
 *  | means the whole result becomes `Nothing` also:
 *  |
 *  | ``` purescript
 *  | f <$> Nothing <*> Just y == Nothing
 *  | f <$> Just x <*> Nothing == Nothing
 *  | f <$> Nothing <*> Nothing == Nothing
 *  | ```
 */
var applyMaybe = new Prelude.Apply(function (_27) {
    return function (x) {
        if (_27 instanceof Just) {
            return Prelude["<$>"](functorMaybe)(_27.value0)(x);
        };
        if (_27 instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match");
    };
}, function () {
    return functorMaybe;
});

/**
 *  | The `Bind` instance allows sequencing of `Maybe` values and functions that
 *  | return a `Maybe` by using the `>>=` operator:
 *  |
 *  | ``` purescript
 *  | Just x >>= f = f x
 *  | Nothing >>= f = Nothing
 *  | ```
 */
var bindMaybe = new Prelude.Bind(function (_28) {
    return function (k) {
        if (_28 instanceof Just) {
            return k(_28.value0);
        };
        if (_28 instanceof Nothing) {
            return Nothing.value;
        };
        throw new Error("Failed pattern match");
    };
}, function () {
    return applyMaybe;
});

/**
 *  | The `Applicative` instance enables lifting of values into `Maybe` with the
 *  | `pure` or `return` function (`return` is an alias for `pure`):
 *  |
 *  | ``` purescript
 *  | pure x :: Maybe _ == Just x
 *  | return x :: Maybe _ == Just x
 *  | ```
 *  |
 *  | Combining `Functor`'s `<$>` with `Apply`'s `<*>` and `Applicative`'s
 *  | `pure` can be used to pass a mixture of `Maybe` and non-`Maybe` typed
 *  | values to a function that does not usually expect them, by using `pure`
 *  | for any value that is not already `Maybe` typed:
 *  |
 *  | ``` purescript
 *  | f <$> Just x <*> pure y == Just (f x y)
 *  | ```
 *  |
 *  | Even though `pure = Just` it is recommended to use `pure` in situations
 *  | like this as it allows the choice of `Applicative` to be changed later
 *  | without having to go through and replace `Just` with a new constructor.
 */
var applicativeMaybe = new Prelude.Applicative(function () {
    return applyMaybe;
}, Just.create);

/**
 *  | The `Monad` instance guarantees that there are both `Applicative` and
 *  | `Bind` instances for `Maybe`. This also enables the `do` syntactic sugar:
 *  |
 *  | ``` purescript
 *  | do
 *  |   x' <- x
 *  |   y' <- y
 *  |   pure (f x' y')
 *  | ```
 *  |
 *  | Which is equivalent to:
 *  |
 *  | ``` purescript
 *  | x >>= (\x' -> y >>= (\y' -> pure (f x' y')))
 *  | ```
 */
var monadMaybe = new Prelude.Monad(function () {
    return applicativeMaybe;
}, function () {
    return bindMaybe;
});

/**
 *  | The `Alt` instance allows for a choice to be made between two `Maybe`
 *  | values with the `<|>` operator, where the first `Just` encountered
 *  | is taken.
 *  |
 *  | ``` purescript
 *  | Just x <|> Just y == Just x
 *  | Nothing <|> Just y == Just y
 *  | Nothing <|> Nothing == Nothing
 *  | ```
 */
var altMaybe = new Control_Alt.Alt(function (l) {
    return function (r) {
        if (l instanceof Nothing) {
            return r;
        };
        return l;
    };
}, function () {
    return functorMaybe;
});

/**
 *  | The `Plus` instance provides a default `Maybe` value:
 *  |
 *  | ``` purescript
 *  | empty :: Maybe _ == Nothing
 *  | ```
 */
var plusMaybe = new Control_Plus.Plus(function () {
    return altMaybe;
}, Nothing.value);

/**
 *  | The `Alternative` instance guarantees that there are both `Applicative` and
 *  | `Plus` instances for `Maybe`.
 */
var alternativeMaybe = new Control_Alternative.Alternative(function () {
    return plusMaybe;
}, function () {
    return applicativeMaybe;
});

/**
 *  | The `MonadPlus` instance guarantees that there are both `Monad` and
 *  | `Alternative` instances for `Maybe`.
 */
var monadPlusMaybe = new Control_MonadPlus.MonadPlus(function () {
    return alternativeMaybe;
}, function () {
    return monadMaybe;
});
module.exports = {
    Nothing: Nothing, 
    Just: Just, 
    isNothing: isNothing, 
    isJust: isJust, 
    fromMaybe: fromMaybe, 
    maybe: maybe, 
    functorMaybe: functorMaybe, 
    applyMaybe: applyMaybe, 
    applicativeMaybe: applicativeMaybe, 
    altMaybe: altMaybe, 
    plusMaybe: plusMaybe, 
    alternativeMaybe: alternativeMaybe, 
    bindMaybe: bindMaybe, 
    monadMaybe: monadMaybe, 
    monadPlusMaybe: monadPlusMaybe, 
    extendMaybe: extendMaybe, 
    semigroupMaybe: semigroupMaybe, 
    showMaybe: showMaybe, 
    eqMaybe: eqMaybe, 
    ordMaybe: ordMaybe
};

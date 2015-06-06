// Generated by psc-make version 0.6.9.3

/**
 *  | This module defines a variant of applicative validation with 
 *  | an `Alternative` instance, for validators which support errors
 *  | with multiple alternatives.
 *  |
 *  | The API is equivalent to `Data.Validation`,
 *  | but uses `Semiring` instead of `Semigroup`.
 */
"use strict";
var Prelude = require("Prelude");
var Control_Alt = require("Control.Alt");
var Control_Plus = require("Control.Plus");
var Control_Alternative = require("Control.Alternative");

/**
 *  | The `V` functor, used for alternative validation
 *  |
 *  | The `Alternative` instance collects multiple failures in
 *  | an arbitrary `Semiring`.
 *  | 
 *  | For example:
 *  | 
 *  | ```purescript
 *  | import Data.Semiring.Free
 *  | 
 *  | validate r :: Person -> V (Free Error) Person 
 *  | validate person = { first: _, last: _, contact: _}
 *  |   <$> validateName person.first
 *  |   <*> validateName person.last
 *  |   <*> (validateEmail person.contact <|> validatePhone person.contact)
 *  | ```
 */
var Valid = (function () {
    function Valid(value0) {
        this.value0 = value0;
    };
    Valid.create = function (value0) {
        return new Valid(value0);
    };
    return Valid;
})();

/**
 *  | The `V` functor, used for alternative validation
 *  |
 *  | The `Alternative` instance collects multiple failures in
 *  | an arbitrary `Semiring`.
 *  | 
 *  | For example:
 *  | 
 *  | ```purescript
 *  | import Data.Semiring.Free
 *  | 
 *  | validate r :: Person -> V (Free Error) Person 
 *  | validate person = { first: _, last: _, contact: _}
 *  |   <$> validateName person.first
 *  |   <*> validateName person.last
 *  |   <*> (validateEmail person.contact <|> validatePhone person.contact)
 *  | ```
 */
var Invalid = (function () {
    function Invalid(value0) {
        this.value0 = value0;
    };
    Invalid.create = function (value0) {
        return new Invalid(value0);
    };
    return Invalid;
})();
var showV = function (__dict_Show_0) {
    return function (__dict_Show_1) {
        return new Prelude.Show(function (_220) {
            if (_220 instanceof Invalid) {
                return "Invalid (" + (Prelude.show(__dict_Show_0)(_220.value0) + ")");
            };
            if (_220 instanceof Valid) {
                return "Valid (" + (Prelude.show(__dict_Show_1)(_220.value0) + ")");
            };
            throw new Error("Failed pattern match");
        });
    };
};

/**
 *  | Unpack the `V` type constructor, providing functions to handle the error
 *  | and success cases.
 */
var runV = function (f) {
    return function (g) {
        return function (_218) {
            if (_218 instanceof Invalid) {
                return f(_218.value0);
            };
            if (_218 instanceof Valid) {
                return g(_218.value0);
            };
            throw new Error("Failed pattern match");
        };
    };
};

/**
 *  | Test whether validation was successful or not
 */
var isValid = function (_219) {
    if (_219 instanceof Valid) {
        return true;
    };
    return false;
};

/**
 *  | Fail with a validation error
 */
var invalid = Invalid.create;
var functorV = new Prelude.Functor(function (f) {
    return function (_221) {
        if (_221 instanceof Invalid) {
            return new Invalid(_221.value0);
        };
        if (_221 instanceof Valid) {
            return new Valid(f(_221.value0));
        };
        throw new Error("Failed pattern match");
    };
});
var applyV = function (__dict_Semiring_3) {
    return new Prelude.Apply(function (_222) {
        return function (_223) {
            if (_222 instanceof Invalid && _223 instanceof Invalid) {
                return new Invalid(Prelude["*"](__dict_Semiring_3)(_222.value0)(_223.value0));
            };
            if (_222 instanceof Invalid) {
                return new Invalid(_222.value0);
            };
            if (_223 instanceof Invalid) {
                return new Invalid(_223.value0);
            };
            if (_222 instanceof Valid && _223 instanceof Valid) {
                return new Valid(_222.value0(_223.value0));
            };
            throw new Error("Failed pattern match");
        };
    }, function () {
        return functorV;
    });
};
var applicativeV = function (__dict_Semiring_4) {
    return new Prelude.Applicative(function () {
        return applyV(__dict_Semiring_4);
    }, Valid.create);
};
var altV = function (__dict_Semiring_5) {
    return new Control_Alt.Alt(function (_224) {
        return function (a) {
            if (_224 instanceof Invalid && a instanceof Invalid) {
                return new Invalid(Prelude["+"](__dict_Semiring_5)(_224.value0)(a.value0));
            };
            if (_224 instanceof Invalid) {
                return a;
            };
            if (_224 instanceof Valid) {
                return new Valid(_224.value0);
            };
            throw new Error("Failed pattern match");
        };
    }, function () {
        return functorV;
    });
};
var plusV = function (__dict_Semiring_2) {
    return new Control_Plus.Plus(function () {
        return altV(__dict_Semiring_2);
    }, new Invalid(Prelude.zero(__dict_Semiring_2)));
};
var alernativeV = function (__dict_Semiring_6) {
    return new Control_Alternative.Alternative(function () {
        return plusV(__dict_Semiring_6);
    }, function () {
        return applicativeV(__dict_Semiring_6);
    });
};
module.exports = {
    isValid: isValid, 
    runV: runV, 
    invalid: invalid, 
    showV: showV, 
    functorV: functorV, 
    applyV: applyV, 
    applicativeV: applicativeV, 
    altV: altV, 
    plusV: plusV, 
    alernativeV: alernativeV
};

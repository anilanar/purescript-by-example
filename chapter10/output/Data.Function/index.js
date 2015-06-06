// Generated by psc-make version 0.6.9.3
"use strict";
var Prelude = require("Prelude");

    function mkFn0(fn) {
      return function() {
        return fn({});
      };
    }
    ;

    function mkFn1(fn) {
      return function(a) {
        return fn(a);
      };
    }
    ;

    function mkFn2(fn) {
      return function(a, b) {
        return fn(a)(b);
      };
    }
    ;

    function mkFn3(fn) {
      return function(a, b, c) {
        return fn(a)(b)(c);
      };
    }
    ;

    function mkFn4(fn) {
      return function(a, b, c, d) {
        return fn(a)(b)(c)(d);
      };
    }
    ;

    function mkFn5(fn) {
      return function(a, b, c, d, e) {
        return fn(a)(b)(c)(d)(e);
      };
    }
    ;

    function mkFn6(fn) {
      return function(a, b, c, d, e, f) {
        return fn(a)(b)(c)(d)(e)(f);
      };
    }
    ;

    function mkFn7(fn) {
      return function(a, b, c, d, e, f, g) {
        return fn(a)(b)(c)(d)(e)(f)(g);
      };
    }
    ;

    function mkFn8(fn) {
      return function(a, b, c, d, e, f, g, h) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h);
      };
    }
    ;

    function mkFn9(fn) {
      return function(a, b, c, d, e, f, g, h, i) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h)(i);
      };
    }
    ;

    function mkFn10(fn) {
      return function(a, b, c, d, e, f, g, h, i, j) {
        return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j);
      };
    }
    ;

    function runFn0(fn) {
      return fn();
    }
    ;

    function runFn1(fn) {
      return function(a) {
        return fn(a);
      };
    }
    ;

    function runFn2(fn) {
      return function(a) {
        return function(b) {
          return fn(a, b);
        };
      };
    }
    ;

    function runFn3(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return fn(a, b, c);
          };
        };
      };
    }
    ;

    function runFn4(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return fn(a, b, c, d);
            };
          };
        };
      };
    }
    ;

    function runFn5(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return fn(a, b, c, d, e);
              };
            };
          };
        };
      };
    }
    ;

    function runFn6(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return fn(a, b, c, d, e, f);
                };
              };
            };
          };
        };
      };
    }
    ;

    function runFn7(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return fn(a, b, c, d, e, f, g);
                  };
                };
              };
            };
          };
        };
      };
    }
    ;

    function runFn8(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return function(h) {
                      return fn(a, b, c, d, e, f, g, h);
                    };
                  };
                };
              };
            };
          };
        };
      };
    }
    ;

    function runFn9(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return function(h) {
                      return function(i) {
                        return fn(a, b, c, d, e, f, g, h, i);
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    }
    ;

    function runFn10(fn) {
      return function(a) {
        return function(b) {
          return function(c) {
            return function(d) {
              return function(e) {
                return function(f) {
                  return function(g) {
                    return function(h) {
                      return function(i) {
                        return function(j) {
                          return fn(a, b, c, d, e, f, g, h, i, j);
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    }
    ;

/**
 *  | The `on` function is used to change the domain of a binary operator.
 *  |
 *  | For example, we can create a function which compares two records based on the values of their `x` properties:
 *  |
 *  | ```purescript
 *  | compareX :: forall r. { x :: Number | r } -> { x :: Number | r } -> Ordering
 *  | compareX = compare `on` _.x
 *  | ```
 */
var on = function (f) {
    return function (g) {
        return function (x) {
            return function (y) {
                return f(g(x))(g(y));
            };
        };
    };
};
module.exports = {
    runFn10: runFn10, 
    runFn9: runFn9, 
    runFn8: runFn8, 
    runFn7: runFn7, 
    runFn6: runFn6, 
    runFn5: runFn5, 
    runFn4: runFn4, 
    runFn3: runFn3, 
    runFn2: runFn2, 
    runFn1: runFn1, 
    runFn0: runFn0, 
    mkFn10: mkFn10, 
    mkFn9: mkFn9, 
    mkFn8: mkFn8, 
    mkFn7: mkFn7, 
    mkFn6: mkFn6, 
    mkFn5: mkFn5, 
    mkFn4: mkFn4, 
    mkFn3: mkFn3, 
    mkFn2: mkFn2, 
    mkFn1: mkFn1, 
    mkFn0: mkFn0, 
    on: on
};

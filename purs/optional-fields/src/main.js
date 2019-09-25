// return a 2-param function because the foreign import has a typeclass constraint 
exports.frgn = function () { 
  return function (x) {
    if (x.a !== undefined) {
      return x.a + x.b
    }
    else {
      return x.b
    }
  } 
} 

// a copy of frgn, but each foreign import from purs needs a corresponding export
// return a 2-param function because the foreign import has a typeclass constraint 
exports.frgn1 = function () { 
  return function (x) {
    if (x.a !== undefined) {
      return x.a + x.b
    }
    else {
      return x.b
    }
  } 
} 

// differs from the other frgn functions in that neither `a` nor `b` are required
// return a 2-param function because the foreign import has a typeclass constraint 
exports.frgn2 = function () { 
  return function (x) {
    var a = x.a || 0;
    var b = x.b || 0;
    return a + b;
  } 
} 

// a copy of frgn, but each foreign import from purs needs a corresponding export
// return a 2-param function because the foreign import has a typeclass constraint 
exports.frgn3 = function () { 
  return function (x) {
    if (x.a !== undefined) {
      return x.a + x.b
    }
    else {
      return x.b
    }
  } 
} 

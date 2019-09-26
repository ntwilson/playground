// a function that takes in an object where property `b` is required, but property `a` is optional
const frgn = function (x) {
  if (x.a !== undefined) {
    return x.a + x.b
  }
  else {
    return x.b
  }
}

// a function that takes in an object where both properties `a` and `b` are optional
const allOptional = function(x) {
  var a = x.a || 0;
  var b = x.b || 0;
  return a + b;
}

// return a 2-param function because the foreign import has a constraint 
exports.frgn = function () { return frgn; } 

// return a 2-param function because the foreign import has a constraint 
exports.frgn1 = function () { return frgn; }

// return a 2-param function because the foreign import has a constraint 
exports.frgn2 = function () { return allOptional; }

// return a 2-param function because the foreign import has a constraint 
exports.frgn3 = function () { return frgn; }

exports.frgn4Impl = frgn;

exports.undefined = undefined

exports.frgn5 = frgn;
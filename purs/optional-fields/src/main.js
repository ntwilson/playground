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

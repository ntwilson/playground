function time(f) { 
  return function() {
    var start = performance.now();
    var ret = f();
    var end = performance.now();
    return {value0: end - start, value1: ret};
  } 
}

function nowMS() { 
  return performance.now();
}

function PSsetTimeout(f) {
  return function(t) {
    return function() { 
      setTimeout(f, t)
    }
  }
}

exports.time = time;
exports.nowMS = nowMS;
exports.setTimeout = PSsetTimeout;
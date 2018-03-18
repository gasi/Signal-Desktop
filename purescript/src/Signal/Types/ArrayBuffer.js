exports.eqImpl = function (a) {
  return function (b) {
    if (a === b) {
      return true;
    }

    if (a.byteLength !== b.byteLength) {
      return false;
    }

    var viewA = new DataView(a);
    var viewB = new DataView(b);

    var numBytes = a.byteLength;
    for (var index = 0; index < a.numBytes; index++) {
      if (viewA.getUint8(index) !== viewB.getUint8(index)) {
        return false;
      }
    }

    return true;
  }
};

exports.regionCodeForNumberImpl = function (phoneNumber) {
  return libphonenumber.getRegionCodeForNumber(phoneNumber);
};

exports.formatImpl = function (phoneNumber) {
  return function (phoneNumberFormat) {
    return libphonenumber.format(phoneNumber, phoneNumberFormat);
  };
};

exports.parseImpl = function (s) {
  return libphonenumber.parse(s);
};

exports.formatStringImpl = function (s) {
  return function (phoneNumberFormat) {
    var phoneNumber = exports.parseImpl(s);
    return exports.formatImpl(phoneNumber)(phoneNumberFormat);
  };
};

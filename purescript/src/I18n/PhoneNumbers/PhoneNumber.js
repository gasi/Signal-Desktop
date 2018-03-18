/* global libphonenumber */

exports.regionCodeForNumberImpl = function (just, nothing, phoneNumber) {
  var regionCode = libphonenumber.getRegionCodeForNumber(phoneNumber);
  if (!regionCode) {
    return nothing;
  }

  return just(regionCode);
};

exports.formatImpl = function (phoneNumberFormat) {
  return function (phoneNumber) {
    return libphonenumber.format(phoneNumber, phoneNumberFormat);
  };
};

exports.parseImpl = function (just, nothing, s) {
  var phoneNumber;
  try {
    phoneNumber = libphonenumber.parse(s);
  } catch (error) {
    return nothing;
  }

  if (!phoneNumber) {
    return nothing;
  }

  return just(phoneNumber);
};

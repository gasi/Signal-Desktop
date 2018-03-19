exports.compareTitleImpl = function (EQ, LT, GT, s1, s2) {
  var result = new Intl.Collator().compare(s1, s2);
  if (result < 0) {
    return LT;
  } else if (result > 0) {
    return GT;
  }

  return EQ;
};

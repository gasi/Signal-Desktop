exports.hashCodeImpl = function (string) {
  if (string.length === 0) {
    return 0;
  }

  var hash = 0;
  for (var i = 0; i < string.length; i++) {
    hash = ((hash << 5) - hash) + string.charCodeAt(i);
    hash = hash & hash; // Convert to 32bit integer
  }

  return Math.abs(hash);
}

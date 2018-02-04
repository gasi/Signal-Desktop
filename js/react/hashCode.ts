const hashCode = (value: string) => {
  if (value.length === 0) {
    return 0;
  }

  let hash = 0;
  for (let i = 0; i < value.length; i++) {
    hash = ((hash << 5) - hash) + value.charCodeAt(i);
    hash = hash & hash; // Convert to 32bit integer
  }

  return Math.abs(hash);
};

window.Whisper.React.hashCode = hashCode;

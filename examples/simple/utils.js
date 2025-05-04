export function formatResult(operation, result) {
  return `${operation} result: ${result}`;
}

export function padLeft(str, length, char = ' ') {
  while (str.length < length) {
    str = char + str;
  }
  return str;
}

export function padRight(str, length, char = ' ') {
  while (str.length < length) {
    str = str + char;
  }
  return str;
}
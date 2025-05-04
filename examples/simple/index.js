import { add, subtract } from './math.js';
import { formatResult } from './utils.js';

const x = 10;
const y = 5;

const addResult = add(x, y);
const subtractResult = subtract(x, y);

console.log(formatResult('Addition', addResult));
console.log(formatResult('Subtraction', subtractResult));

export default {
  addResult,
  subtractResult
};
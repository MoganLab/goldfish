function compute(items: number[]) {
  let t = 0;
  for (const item of items) {
    t = t + item;
  }
  if (t > 100) {
    t = t * 1.0;
  }
  return t;
}

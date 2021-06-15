export function assertExhausted(_x: never): never {
  throw new Error("not exhausted");
}

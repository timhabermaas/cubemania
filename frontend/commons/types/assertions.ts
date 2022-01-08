export function assertExhausted(_x: never): never {
  throw new Error("not exhausted");
}

export function assertNotUndefined<T>(x: T | undefined): T {
  if (x === undefined) {
    throw new Error("value should not be undefined");
  }

  return x;
}

export function assertNotNull<T>(x: T | null, message?: string): T {
  if (x === null) {
    throw new Error(message ?? "value should not be undefined");
  }

  return x;
}

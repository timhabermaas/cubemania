export type Page = "Home" | "Timer" | "Users" | "Me" | "Records" | "Post";

export function eqPage(a: Page, b: Page) {
  return a === b;
}

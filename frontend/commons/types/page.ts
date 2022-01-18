export type Page = "Home" | "Timer" | "Users" | "Me" | "Records" | "Post";

export function eqPage(a: Page, b: Page) {
  return a === b;
}

export function pageFromPathname(pathName: string): Page | undefined {
  switch (pathName) {
    case "/":
      return "Home";
    case "/users":
      return "Users";
    case "/puzzles/[puzzleId]/records":
    case "/records/[recordId]":
      return "Records";
    case "/_error":
      return "Home";
    default:
      return undefined;
  }
}

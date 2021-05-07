export function userPath(slug: string): string {
  return `/users/${slug}`;
}

export const timer3x3x3Path: string = "/puzzles/3x3x3/timer";

export const records3x3x3Path: string = "/puzzles/3x3x3/records";

export const usersPath: string = "/users";

export const homePath: string = "/";

export function postPath(postId: number): string {
  return `/posts/${postId}#comments`;
}

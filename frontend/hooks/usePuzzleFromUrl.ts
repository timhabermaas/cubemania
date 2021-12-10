import { useSingleQueryParam } from "./useSingleQueryParam";

export function usePuzzleFromUrl(): string {
  const q = useSingleQueryParam("puzzleId");
  if (q === null) {
    throw new Error("routing error");
  } else {
    return q;
  }
}

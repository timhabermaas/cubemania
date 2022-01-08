import { assertNotNull } from "../commons/types/assertions";
import { useSingleQueryParam } from "./useSingleQueryParam";

export function usePuzzleFromUrl(): string {
  return assertNotNull(useSingleQueryParam("puzzleId"), "routing error");
}

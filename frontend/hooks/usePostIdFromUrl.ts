import { assertNotNull } from "../commons/types/assertions";
import { useSingleQueryParam } from "./useSingleQueryParam";

export function usePostIdFromUrl(): string {
  return assertNotNull(useSingleQueryParam("postId"), "routing error");
}

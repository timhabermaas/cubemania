import { assertNotNull } from "../commons/types/assertions";
import { useSingleQueryParam } from "./useSingleQueryParam";

export function useRecordIdFromUrl(): string {
  return assertNotNull(useSingleQueryParam("recordId"), "routing error");
}

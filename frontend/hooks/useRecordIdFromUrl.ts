import { useSingleQueryParam } from "./useSingleQueryParam";

export function useRecordIdFromUrl(): string {
  const q = useSingleQueryParam("recordId");

  if (q === null) {
    throw new Error("routing error");
  } else {
    return q;
  }
}

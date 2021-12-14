import { RecordTypes } from "../models/recordTypes";
import { useSingleQueryParam } from "./useSingleQueryParam";

export function useRecordTypeFromUrl(): string {
  return useSingleQueryParam("type") ?? RecordTypes[1].short;
}

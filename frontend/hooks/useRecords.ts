import { useQuery } from "react-query";
import { get } from "../commons/http/HttpClient";
import { PaginatedResponse } from "../commons/types/PaginatedResponse";

export interface Record {
  id: number;
  rank: number;
  time: number;
  comment: string;
  set_at: string;
  user_name: string;
  user_slug: string;
}

interface RecordResponse {
  records: PaginatedResponse<Record>;
}

export function useRecords(type: string, page: number, puzzleSlug: string) {
  return useQuery<RecordResponse>(
    ["records", puzzleSlug, type, page],
    async () =>
      get(`/api/records?type=${type}&page=${page}&puzzle_slug=${puzzleSlug}`),
    { keepPreviousData: true }
  );
}

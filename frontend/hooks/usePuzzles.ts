import { useQuery } from "react-query";
import { get } from "../commons/http/HttpClient";

export interface Puzzle {
  css_position: number;
  id: number;
  name: string;
  slug: string;
}

export interface Kind {
  id: number;
  name: string;
  css_position: number;
  puzzles: Puzzle[];
}

export function usePuzzles() {
  return useQuery<Kind[]>(
    "allPuzzles",
    async () => get<Kind[]>("/api/puzzles"),
    { staleTime: Infinity }
  );
}

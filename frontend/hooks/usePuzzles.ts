import { useQuery } from "react-query";

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
    async () => {
      const response = await fetch("/api/puzzles");
      return await response.json();
    },
    { staleTime: Infinity }
  );
}

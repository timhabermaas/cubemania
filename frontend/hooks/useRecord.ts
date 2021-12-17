import { useQuery } from "react-query";

interface Single {
  time: number;
  scramble: string;
  comment: string;
}

export interface Record {
  id: number;
  time: number;
  amount: number;
  set_at: string;
  user_name: string;
  user_slug: string;
  puzzle_name: string;
  kind_short_name?: string;
  puzzle_css_position: string;
  kind_css_position: string;
  singles: Single[];
}

export function useRecord(recordId: number) {
  return useQuery<Record>(["record", recordId], async () => {
    let response = await fetch(`/api/records/${recordId}`);
    return await response.json();
  });
}

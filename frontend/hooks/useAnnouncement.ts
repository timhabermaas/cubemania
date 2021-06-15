import { useQuery } from "react-query";

type AnnouncementResponse = null | {
  id: number;
  title: string;
  content: string;
  comments_count: number;
};

export function useAnnouncement() {
  return useQuery<AnnouncementResponse, Error>(["announcement"], async () => {
    const r = await fetch("/api/announcement");
    return await r.json();
  });
}

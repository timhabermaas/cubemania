import { useQuery } from "react-query";
import { get } from "../commons/http/HttpClient";

type AnnouncementResponse = null | {
  id: number;
  title: string;
  content: string;
  comments_count: number;
};

export function useAnnouncement() {
  return useQuery(["announcement"], async () =>
    get<AnnouncementResponse>("/api/announcement")
  );
}

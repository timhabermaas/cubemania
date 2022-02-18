import { useQuery } from "react-query";
import { get } from "../commons/http/HttpClient";

interface Comment {
  id: number;
  content: string;
  created_at: string;
  user_name: string;
  user_slug: string;
}

interface Post {
  id: number;
  user_name: string;
  user_slug: string;
  title: string;
  content: string;
  created_at: string;
  comments: Comment[];
}

export function usePost(postId: number) {
  return useQuery<Post>(
    ["post", postId],
    async () => await get<Post>(`/api/posts/${postId}`)
  );
}

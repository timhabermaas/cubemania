import { useMutation, useQueryClient } from "react-query";
import { delete_ } from "../commons/http/HttpClient";

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

export function useDeleteComment(postId: number, jwtToken?: string) {
  const queryClient = useQueryClient();

  return useMutation(
    async (commentId: number) => {
      console.log(`deleting comment ${commentId}`);
      const result = await delete_<Post>(
        `/api/comments/${commentId}`,
        jwtToken
      );
      console.log(`comment ${commentId} deleted`);
      return result;
    },
    {
      onSuccess: () => {
        console.log(`invalidating post ${postId}`);
        queryClient.invalidateQueries(["post", postId]);
        console.log(`posts ${postId} invalidated`);
      },
    }
  );
}

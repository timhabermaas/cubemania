import { useMutation, useQueries, useQueryClient } from "react-query";
import { post } from "../commons/http/HttpClient";

export function useCreateComment(postId: number, jwtToken?: string) {
  const queryClient = useQueryClient();

  return useMutation(
    async (content: string) => {
      const result = await post(
        `/api/posts/${postId}/comments`,
        { content },
        jwtToken
      );
      return result;
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(["post", postId]);
      },
    }
  );
}

import { useMutation, useQueryClient } from "react-query";
import { put } from "../commons/http/HttpClient";

export function useBlockUser(jwtToken?: string) {
  const queryClient = useQueryClient();

  return useMutation(
    async (slug: string) => {
      return await put(`/api/users/${slug}/block`, jwtToken);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries("records");
      },
    }
  );
}

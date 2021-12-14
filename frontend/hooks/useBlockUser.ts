import { useMutation, useQueryClient } from "react-query";

export function useBlockUser(jwtToken?: string) {
  const queryClient = useQueryClient();

  return useMutation(
    async (slug: string) => {
      const headers = new Headers();
      if (jwtToken) {
        headers.set("Authorization", `Bearer ${jwtToken}`);
      }
      const r = await fetch(`/api/users/${slug}/block`, {
        headers,
        method: "PUT",
      });
      return await r.json();
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries("records");
      },
    }
  );
}

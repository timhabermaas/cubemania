import { useQuery, useQueryClient } from "react-query";

export type Role = "admin" | "moderator" | "user";

interface MeResponse {
  current_user?: {
    name: string;
    slug: string;
    role: Role;
  };
}

export function useMe(jwtToken?: string) {
  const queryClient = useQueryClient();

  return useQuery<MeResponse, Error>(
    ["me"],
    async () => {
      const headers = new Headers();
      if (jwtToken) {
        headers.set("Authorization", `Bearer ${jwtToken}`);
      }
      const r = await fetch("/api/me", {
        headers,
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

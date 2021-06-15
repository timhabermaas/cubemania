import { useQuery } from "react-query";

interface MeResponse {
  current_user?: { name: string; slug: string };
}

export function useMe(jwtToken?: string) {
  return useQuery<MeResponse, Error>(["me"], async () => {
    const headers = new Headers();
    if (jwtToken) {
      headers.set("Authorization", `Bearer ${jwtToken}`);
    }
    const r = await fetch("/api/me", {
      headers,
    });
    return await r.json();
  });
}

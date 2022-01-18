import { useQuery } from "react-query";
import { get, NotFoundError } from "../commons/http/HttpClient";

export type Role = "admin" | "moderator" | "user";

interface MeResponse {
  current_user?: {
    name: string;
    slug: string;
    role: Role;
  };
}

export function useMe(jwtToken?: string) {
  return useQuery(["me"], async () => {
    try {
      return await get<MeResponse>("/api/me", jwtToken);
    } catch (e: unknown) {
      if (e instanceof NotFoundError) {
        return {};
      } else {
        throw e;
      }
    }
  });
}

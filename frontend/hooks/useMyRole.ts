import { Role, useMe } from "./useMe";

export function useMyRole(jwtToken?: string): Role | undefined {
  const { data } = useMe(jwtToken);

  return data && data.current_user?.role;
}

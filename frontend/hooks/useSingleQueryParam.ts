import { useRouter } from "next/router";

export function useSingleQueryParam(name: string): string | null {
  const router = useRouter();
  const param = router.query[name];
  if (param !== undefined) {
    if (typeof param === "string") {
      return param;
    } else if (Array.isArray(param)) {
      return param[0];
    }
  }

  return null;
}

import { InfiniteData, useInfiniteQuery, useQuery } from "react-query";

interface PaginatedResponse<T> {
  items: Array<T>;
  page: number;
  item_count: number;
  next_page: number | null;
}

interface SimpleUser {
  name: string;
  slug: string;
  singles_count: number;
}

interface UsersResponse {
  users: PaginatedResponse<SimpleUser>;
}

interface MaxSinglesRecordResponse {
  max_singles_record: number;
}

interface UserIndexData {
  data?: { singlesCount: number; users: InfiniteData<UsersResponse> };
  error?: Error;
  isLoading: boolean;
  fetchNextPage: () => void;
  hasNextPage?: boolean;
}

export function useUsersIndexData({
  searchTerm,
  jwtToken,
}: {
  searchTerm: string | null;
  jwtToken?: string;
}): UserIndexData {
  const { isLoading: singlesCountLoading, data: singlesCountData } = useQuery<
    MaxSinglesRecordResponse,
    Error
  >(
    ["max_singles_record"],
    async () => {
      const headers = new Headers();
      headers.set("Authorization", `Bearer ${jwtToken}`);
      const r = await fetch("/api/max_singles_record", {
        headers,
      });
      return await r.json();
    },
    // Setting stale time to infinity since this data rarely changes, no need
    // to refetch the data.
    { staleTime: Infinity }
  );

  const { data, isLoading, hasNextPage, fetchNextPage } = useInfiniteQuery<
    UsersResponse,
    Error
  >(
    ["users", searchTerm],
    async ({ pageParam = 0 }) => {
      const queryParam = new URLSearchParams({
        page: pageParam,
      });
      if (searchTerm !== null) {
        queryParam.append("q", searchTerm);
      }
      const headers = new Headers();
      headers.set("Authorization", `Bearer ${jwtToken}`);
      const r = await fetch(`/api/users?${queryParam}`, {
        headers,
      });
      return await r.json();
    },
    {
      getNextPageParam: (lastPage, _pages) => lastPage.users.next_page,
      // keepPreviousData: true prevents flickering when typing in search box.
      keepPreviousData: true,
    }
  );

  return {
    data: data &&
      singlesCountData && {
        singlesCount: singlesCountData.max_singles_record,
        users: data,
      },
    isLoading: isLoading || singlesCountLoading,
    hasNextPage,
    fetchNextPage,
  };
}

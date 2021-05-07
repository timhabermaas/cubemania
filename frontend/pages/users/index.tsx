import { GetServerSideProps } from "next";
import React, { useState } from "react";
import { useCallback } from "react";
import { userPath } from "../../commons/path";
import { Layout } from "../../components/Layout";
import { useUsersIndexData } from "../../hooks/useUsersIndexData";

interface UserProps {
  size: number;
  url: string;
  singlesCount: number;
  maxSinglesCount: number;
  name: string;
}

function User(props: UserProps) {
  const fontSizeEm = (props.singlesCount / props.maxSinglesCount) * 1.4 + 0.6;

  return (
    <>
      <li style={{ fontSize: `${fontSizeEm}em` }}>
        <a href={props.url}>{props.name}</a>{" "}
        <small className="singles">{props.singlesCount}</small>
      </li>{" "}
    </>
  );
}

interface UsersProps {
  jwtToken?: string;
}

export default function Users(props: UsersProps) {
  const [searchTerm, setSearchTerm] = useState<string | null>(null);

  const { data, hasNextPage, fetchNextPage } = useUsersIndexData({
    searchTerm,
    jwtToken: props.jwtToken,
  });

  const showMore = useCallback((event) => {
    event.preventDefault();
    fetchNextPage();
  }, []);

  return (
    <Layout jwtToken={props.jwtToken} page={"Users"}>
      <form
        acceptCharset="UTF-8"
        action="/users"
        id="users-search"
        method="get"
      >
        <input
          autoSave="users-search"
          id="q"
          name="q"
          placeholder="Search"
          results={5}
          type="search"
          onInput={(e) => {
            if (e.currentTarget.value === "") {
              setSearchTerm(null);
            } else {
              setSearchTerm(e.currentTarget.value);
            }
          }}
        />
      </form>
      <ul id="users" className="users">
        {data &&
          data.users.pages.map((page, i) => (
            <React.Fragment key={i + 1}>
              {page.users.items.map((u) => (
                <User
                  key={u.slug}
                  name={u.name}
                  singlesCount={u.singles_count}
                  maxSinglesCount={data.singlesCount}
                  url={userPath(u.slug)}
                  size={10}
                />
              ))}
            </React.Fragment>
          ))}
      </ul>
      {hasNextPage && (
        <div className="pagination">
          <a href="#" onClick={showMore}>
            Show more
          </a>
        </div>
      )}
    </Layout>
  );
}

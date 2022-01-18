import React from "react";
import {
  records3x3x3Path,
  timer3x3x3Path,
  userPath,
  usersPath,
} from "../commons/path";
import { eqPage, Page } from "../commons/types/page";
import { useMe } from "../hooks/useMe";
import Link from "next/link";

interface HeaderProps {
  page?: Page;
  jwtToken?: string;
}

interface NavItemProps {
  title: string;
  url: string;
  page: Page;
  currentPage?: Page;
  pushedRight?: boolean;
}

function NavItem(props: NavItemProps) {
  const selected = props.currentPage && eqPage(props.page, props.currentPage);

  return (
    <li
      className={`${selected ? "selected" : ""} ${
        props.pushedRight ? "session" : ""
      }`}
    >
      <Link href={props.url}>
        <a>{props.title}</a>
      </Link>
    </li>
  );
}

export function Header(props: HeaderProps) {
  const { data } = useMe(props.jwtToken);

  const isLoggedIn = !!data?.current_user;

  return (
    <>
      <header className="main">
        <div className="center">
          <h1>
            <Link href="/">
              <a>Cubemania</a>
            </Link>
          </h1>
          <q>Save The World – Solve The Puzzle</q>
        </div>
      </header>
      <nav className="main">
        <ul>
          <NavItem url="/" title="Home" page="Home" currentPage={props.page} />
          <NavItem
            url={timer3x3x3Path}
            title="Timer"
            page="Timer"
            currentPage={props.page}
          />
          <NavItem
            url={usersPath}
            title="Users"
            page="Users"
            currentPage={props.page}
          />
          <NavItem
            url={records3x3x3Path}
            title="Records"
            page="Records"
            currentPage={props.page}
          />

          {data?.current_user && (
            <>
              <li className="session">
                <a href="/logout">Logout</a>
              </li>
              <NavItem
                pushedRight
                url={userPath(data.current_user.slug)}
                title={`${data.current_user.name}'s Profile`}
                page="Me"
                currentPage={props.page}
              />
            </>
          )}
          {!isLoggedIn && (
            <li className="session">
              <a href="/login">Login</a>
            </li>
          )}
        </ul>
      </nav>
    </>
  );
}

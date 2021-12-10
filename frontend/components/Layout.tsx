import React from "react";
import { Flash } from "./Flash";
import { Footer } from "./Footer";
import { Header } from "./Header";
import Head from "next/head";
import { Page } from "../commons/types/page";
import { assertExhausted } from "../commons/types/assertions";

interface LayoutProps {
  jwtToken?: string;
  puzzleNav?: JSX.Element;
  page: Page;
}

function pageTitle(page: Page): string {
  switch (page) {
    case "Home":
      return "Home · Cubemania";
    case "Timer":
      return "Timer · Cubemania";
    case "Post":
      return "Post · Cubemania";
    case "Users":
      return "Users · Cubemania";
    case "Me":
      return "User · Cubemania";
    case "Records":
      return "Records · Cubemania";
    default:
      return assertExhausted(page);
  }
}

export const Layout: React.FC<LayoutProps> = (props) => {
  return (
    <div className="container">
      <Head>
        <title>{pageTitle(props.page)}</title>
      </Head>
      <Header jwtToken={props.jwtToken} page={props.page} />
      {props.puzzleNav}
      <Flash />
      <section id="content">
        <div className="center">{props.children}</div>
      </section>
      <Footer />
    </div>
  );
};

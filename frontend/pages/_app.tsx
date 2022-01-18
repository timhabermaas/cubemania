import App, { AppProps, AppContext } from "next/app";
import React, { useEffect, useRef } from "react";
import { QueryClient, QueryClientProvider } from "react-query";
import { ReactQueryDevtools } from "react-query/devtools";
import "../styles/globals.css.scss";
import { useRouter } from "next/router";
import { NotFoundErrorBoundary } from "../components/NotFoundErrorBoundary";
import { Layout } from "../components/Layout";
import { pageFromPathname } from "../commons/types/page";
import { PuzzleNav } from "../components/PuzzleNav";
import { ErrorBoundary } from "../commons/ErrorBoundary";

function MyApp({ Component, pageProps }: AppProps) {
  const router = useRouter();
  const currentMenuPage = pageFromPathname(router.pathname);
  const queryClientRef = useRef<QueryClient>();
  const jwtToken = useRef<string>(pageProps.jwtToken);

  // Save jwt token to component tree using ref to prevent jwt token from
  // clearing when switching between pages (pageProps.jwtToken is undefined in
  // this case since jwt token is only ever generated when hitting the server.)
  useEffect(() => {
    if (pageProps.jwtToken !== undefined) {
      jwtToken.current = pageProps.jwtToken;
    }
  }, [pageProps.jwtToken]);

  if (!queryClientRef.current) {
    queryClientRef.current = new QueryClient({
      defaultOptions: {
        queries: { useErrorBoundary: true, retry: false },
        mutations: { useErrorBoundary: true },
      },
    });
  }

  return (
    <ErrorBoundary>
      <QueryClientProvider client={queryClientRef.current}>
        <ReactQueryDevtools initialIsOpen={false} />
        <Layout
          jwtToken={jwtToken.current}
          page={currentMenuPage}
          puzzleNav={
            router.pathname === "/puzzles/[puzzleId]/records" ? (
              <PuzzleNav />
            ) : undefined
          }
        >
          <NotFoundErrorBoundary router={router}>
            <Component {...pageProps} jwtToken={jwtToken.current} />
          </NotFoundErrorBoundary>
        </Layout>
      </QueryClientProvider>
    </ErrorBoundary>
  );
}

MyApp.getInitialProps = async (appContext: AppContext) => {
  const appProps = await App.getInitialProps(appContext);

  // Server side only.
  if (typeof window === "undefined") {
    const cookie = appContext.ctx.req!.headers.cookie ?? "";
    const token = await fetchJwtToken(cookie);

    return { ...appProps, pageProps: { jwtToken: token } };
  } else {
    return { ...appProps };
  }
};

async function fetchJwtToken(cookie: string): Promise<string | undefined> {
  const headers = new Headers();
  headers.append("Accept", "application/json");
  headers.append("Content-Type", "application/json");
  headers.set("Cookie", cookie);

  const response = await fetch("http://web:3000/session", { headers });
  if (response.status !== 200 && response.status !== 404) {
    throw new Error("TODO");
  }

  if (response.status === 404) {
    return undefined;
  } else {
    return (await response.json()).token;
  }
}

export default MyApp;

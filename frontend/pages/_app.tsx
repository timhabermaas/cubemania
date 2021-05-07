import App, { AppProps, AppContext } from "next/app";
import { useEffect, useRef } from "react";
import { QueryClient, QueryClientProvider } from "react-query";
import { ReactQueryDevtools } from "react-query/devtools";
import "../styles/globals.css.scss";

function MyApp({ Component, pageProps }: AppProps) {
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
    queryClientRef.current = new QueryClient();
  }

  return (
    <QueryClientProvider client={queryClientRef.current}>
      <ReactQueryDevtools initialIsOpen={false} />
      <Component {...pageProps} jwtToken={jwtToken.current} />
    </QueryClientProvider>
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

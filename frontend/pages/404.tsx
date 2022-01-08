import React from "react";
import { Layout } from "../components/Layout";

export default function Custom404() {
  return (
    <Layout page={"Records"}>
      <h1>The page you were looking for doesn't exist.</h1>
      <p>You may have mistyped the address or the page may have moved.</p>
    </Layout>
  );
}

import React, { ReactNode } from "react";
import Error from "next/error";

export class ErrorBoundary extends React.Component<
  { children: ReactNode },
  { hasError: boolean }
> {
  public state = { hasError: false };

  static getDerivedStateFromError(_error: unknown) {
    return { hasError: true };
  }

  render() {
    if (this.state.hasError) {
      return <Error statusCode={500}></Error>;
    }

    return this.props.children;
  }
}

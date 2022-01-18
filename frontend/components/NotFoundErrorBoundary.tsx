import { NextRouter } from "next/router";
import React, { ReactNode } from "react";
import { NotFoundError } from "../commons/http/HttpClient";
import { NotFound } from "../commons/NotFound";

export class NotFoundErrorBoundary extends React.Component<
  { children: ReactNode; router: NextRouter },
  { hasError: boolean }
> {
  constructor(props: { children: ReactNode; router: NextRouter }) {
    super(props);
    this.onRouteChangeComplete = this.onRouteChangeComplete.bind(this);
  }

  public state = { hasError: false };

  static getDerivedStateFromError(_error: unknown) {
    return { hasError: true };
  }

  componentDidCatch(error: unknown, _errorInfo: unknown) {
    if (!(error instanceof NotFoundError)) {
      throw error;
    }
  }

  onRouteChangeComplete() {
    this.setState({ hasError: false });
  }

  componentDidMount() {
    // NOTE: Listening on route changes and clearing the error state is
    // necessary, otherwise using the back button after seeing a 404 error
    // won't have any effect.
    // See also https://stackoverflow.com/q/48121750.
    this.props.router.events.on(
      "routeChangeComplete",
      this.onRouteChangeComplete
    );
  }

  componentWillUnmount() {
    this.props.router.events.off(
      "routeChangeComplete",
      this.onRouteChangeComplete
    );
  }

  render() {
    if (this.state.hasError) {
      return <NotFound></NotFound>;
    }

    return this.props.children;
  }
}

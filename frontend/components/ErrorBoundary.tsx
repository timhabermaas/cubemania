import { NextRouter } from "next/router";
import React, { ReactNode } from "react";
import Error404 from "../pages/404";

export class ErrorBoundary extends React.Component<
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

  onRouteChangeComplete() {
    this.setState({ hasError: false });
  }

  componentDidMount() {
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
      return <Error404></Error404>;
    }

    return this.props.children;
  }
}

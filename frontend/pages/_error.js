// Overwriting _error page since not doing so would trigger static site
// generation of 500 which would call `_app.getInitialProps` which would then
// try to access some URL which isn't necessary online during the build,
// breaking the build.
// See https://github.com/vercel/next.js/issues/23128

function Error() {
  return <p>An error occurred on client</p>;
}

Error.getInitialProps = ({ res, err }) => {
  const statusCode = res ? res.statusCode : err ? err.statusCode : 404;
  return { statusCode };
};

export default Error;

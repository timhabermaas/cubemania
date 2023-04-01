export class UnauthorizedError extends Error {
  constructor() {
    super("HTTP request returned 401");
    this.name = "UnauthorizedError";
  }
}

export class NotFoundError extends Error {
  constructor() {
    super("HTTP request returned 404");
    this.name = "NotFoundError";
  }
}

export class RequestError extends Error {
  statusCode: number;
  body: unknown;

  constructor(statusCode: number, body: unknown) {
    super(
      `HTTP request failed with ${statusCode}. Response: ${JSON.stringify(
        body
      )}`
    );
    this.name = "RequestError";
    this.statusCode = statusCode;
    this.body = body;
  }
}

async function request<T>(
  url: string,
  method: string,
  jsonBody?: unknown,
  jwtToken?: string
): Promise<T> {
  const headers = new Headers();
  if (jwtToken) {
    headers.set("Authorization", `Bearer ${jwtToken}`);
  }
  headers.set("Content-Type", "application/json;charset=utf8");

  const response = await fetch(url, {
    body: jsonBody ? JSON.stringify(jsonBody) : undefined,
    method: method,
    headers,
  });
  const content = await response.json();

  if (response.ok) {
    return content;
  } else if (response.status === 401) {
    throw new UnauthorizedError();
  } else if (response.status === 404) {
    throw new NotFoundError();
  } else {
    throw new RequestError(response.status, content);
  }
}

export async function get<T = unknown>(
  url: string,
  jwtToken?: string
): Promise<T> {
  return request(url, "GET", undefined, jwtToken);
}

export async function post<T = unknown>(
  url: string,
  jsonBody: unknown,
  jwtToken?: string
): Promise<T> {
  return request(url, "POST", jsonBody, jwtToken);
}

export async function put<T = unknown>(
  url: string,
  jwtToken?: string
): Promise<T> {
  return request(url, "PUT", undefined, jwtToken);
}

export async function delete_<T = unknown>(
  url: string,
  jwtToken?: string
): Promise<T> {
  return request(url, "DELETE", undefined, jwtToken);
}

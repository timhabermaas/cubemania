# Install dependencies only when needed
FROM node:14-alpine AS deps
# Check https://github.com/nodejs/docker-node/tree/b4117f9333da4138b03a546ec926ef50a31506c3#nodealpine to understand why libc6-compat might be needed.
RUN apk add --no-cache libc6-compat
WORKDIR /app
COPY package.json yarn.lock ./
RUN yarn install --frozen-lockfile

FROM node:14-alpine AS dev-runner
WORKDIR /app

COPY --from=deps /app/node_modules ./node_modules

EXPOSE 4000

# Next.js collects completely anonymous telemetry data about general usage.
# Learn more here: https://nextjs.org/telemetry
# Uncomment the following line in case you want to disable telemetry.
ENV NEXT_TELEMETRY_DISABLED 1

CMD ["yarn", "run", "dev", "-p", "4000"]

# Rebuild the source code only when needed
FROM node:14-alpine AS prod-builder
WORKDIR /app
COPY . .
COPY --from=deps /app/node_modules ./node_modules
RUN yarn build && yarn install --production --ignore-scripts --prefer-offline



# Production image, copy all the files and run next
FROM node:14-alpine AS prod-runner
WORKDIR /app

ENV NODE_ENV production

# You only need to copy next.config.js if you are NOT using the default configuration
# COPY --from=builder /app/next.config.js ./
COPY --from=prod-builder /app/public ./public
COPY --from=prod-builder /app/.next ./.next
COPY --from=prod-builder /app/node_modules ./node_modules
COPY --from=prod-builder /app/package.json ./package.json


EXPOSE 4000

# Next.js collects completely anonymous telemetry data about general usage.
# Learn more here: https://nextjs.org/telemetry
# Uncomment the following line in case you want to disable telemetry.
ENV NEXT_TELEMETRY_DISABLED 1

CMD ["yarn", "start", "-p", "4000"]

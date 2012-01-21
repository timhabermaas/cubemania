# Be sure to restart your server when you modify this file.

# Your secret key for verifying the integrity of signed cookies.
# If you change this key, all old signed cookies will become invalid!
# Make sure the secret is at least 30 characters and all random,
# no regular words or you'll be exposed to dictionary attacks.
Rails.application.config.secret_token = ENV['SECRET_TOKEN'] || '51e5bc788df528014ac1f5fc8d0d8617a204e0579a904a3832f29019f353ed4a9c77371b6de94a7cfa9c5655a5a6e5e1daaf65224b8efda73b7b12a4c205b953'

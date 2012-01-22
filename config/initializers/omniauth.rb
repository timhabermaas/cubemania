Rails.application.config.middleware.use OmniAuth::Builder do
  provider :twitter, ENV["TWITTER_CONSUMER_KEY"], ENV["TWITTER_CONSUMER_SECRET"]
  provider :facebook, ENV["FACEBOOK_APP_KEY"], ENV["FACEBOOK_APP_SECRET"], :scope => "publish_stream,publish_actions,user_likes"
  provider :developer unless Rails.env.production?
end

OmniAuth.config.test_mode = true if Rails.env.test?

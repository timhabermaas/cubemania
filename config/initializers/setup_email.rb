ActionMailer::Base.smtp_settings = {
  :enable_starttls_auto => true,
  :address => "smtp.gmail.com",
  :port => 587,
  :domain => "cubemania.org",
  :user_name => "info@cubemania.org",
  :password => ENV['EMAIL_PASSWORD'] || "blub",
  :authentication => :plain
}

Mail.register_interceptor(DevelopmentMailInterceptor) if Rails.env.development?

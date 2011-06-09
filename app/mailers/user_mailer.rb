class UserMailer < ActionMailer::Base
  default :from => 't.habermaas@gmail.com'

  def welcome_mail(user)
    @user = user
    mail :to => "#{user.name} <#{user.email}>", :subject => 'Welcome to Cubemania'
  end

  def password_reset(user, password)
    @user = user
    @password = password
    mail :to => "#{user.name} <#{user.email}>", :subject => 'New Password For Cubemania'
  end

  def match_mail(user, match)
    recipients user.email
    from 'Cubemania <info@cubemania.org>'
    subject "#{match.user.name} challenged you"
    sent_on Time.now
    body :user => user, :match => match
  end
end
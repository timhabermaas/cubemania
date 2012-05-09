class UserMailer < ActionMailer::Base
  default :from => 'Cubemania <info@cubemania.org>'

  def welcome(user) # TODO add more text
    @user = user
    mail :to => "#{user.name} <#{user.email}>", :subject => 'Welcome to Cubemania'
  end

  def reset_password(user, password)
    @user = user
    @password = password
    mail :to => "#{user.name} <#{user.email}>", :subject => 'New Password for Cubemania'
  end

  def match_mail(user, match)
    recipients user.email
    from 'Cubemania <info@cubemania.org>'
    subject "#{match.user.name} challenged you"
    sent_on Time.now
    body :user => user, :match => match
  end
end
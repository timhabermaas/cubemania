class UserMailer < ActionMailer::Base
  def welcome_mail(user)
    recipients  user.email
    from        'cubemania <cubemania.org@gmail.com>'
    subject "Welcome to Cubemania"
    sent_on Time.now
    body :user => user
  end
  
  def password_reset_mail(user, password)
    recipients user.email
    from 'cubemania <cubemania.org@gmail.com>'
    subject "New Password for Cubemania"
    sent_on Time.now
    body :password => password, :user => user
  end
end
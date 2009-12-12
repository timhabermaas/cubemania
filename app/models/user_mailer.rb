class UserMailer < ActionMailer::Base
  def welcome_mail(user)
    recipients  user.email
    from        'Cubemania <info@cubemania.org>'
    subject "Welcome to Cubemania"
    sent_on Time.now
    body :user => user
  end
  
  def password_reset_mail(user, password)
    recipients user.email
    from 'Cubemania <password@cubemania.org>'
    subject "New Password for Cubemania"
    sent_on Time.now
    body :password => password, :user => user
  end
  
  def match_mail(user, match)
    recipients user.email
    from 'Cubemania <info@cubemania.org>'
    subject "#{match.user.name} challenged you"
    sent_on Time.now
    body :user => user, :match => match
  end
end
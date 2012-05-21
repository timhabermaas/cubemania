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
end

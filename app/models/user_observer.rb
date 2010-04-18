class UserObserver < ActiveRecord::Observer
  observe :user
  def after_create(user)
    #UserMailer.deliver_welcome_mail(user)
  end
end
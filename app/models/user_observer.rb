class UserObserver < ActiveRecord::Observer
  def after_create(user)
    #UserMailer.deliver_welcome_mail(user)
  end
end
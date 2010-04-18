class PasswordRecoveriesController < ApplicationController
  logout :except => []
  skip_login

  def show
    @recovery = Recovery.new
  end

  def create
    @recovery = Recovery.new params[:recovery]
    if user = @recovery.validate
      user.reset_password!
      UserMailer.password_reset(user, user.password).deliver
      if user.save
        flash[:notice] = "Your new password has been sent to #{@recovery.email}."
        redirect_to root_path
      else
        render :show
      end
    else
      render :show
    end
  end
end

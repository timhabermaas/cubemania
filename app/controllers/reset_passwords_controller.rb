class ResetPasswordsController < ApplicationController
  skip_load_and_authorize_resource
  logout :except => []
  skip_login

  def new
  end

  def create
    user = User.where("lower(email) = ?", params[:reset_password][:email].downcase).first
    if user
      UserMailer.reset_password(user, user.reset_password!).deliver
      redirect_to root_path, :notice => "Email sent successfully."
    else
      flash.now.notice = "Email does not exist."
      render :new
    end
  end
end

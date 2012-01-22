class SessionsController < ApplicationController
  skip_load_and_authorize_resource
  skip_login :except => :destroy
  logout :only => [:new, :create]

  def new
    @login = Login.new
  end

  def create
    @login = Login.new params[:login]
    if self.current_user = @login.validate
      flash[:notice] = "Hello #{current_user.name}, you are now logged in"
      redirect_back root_path
    else
      render :new
    end
  end

  def destroy
    self.current_user = nil
    flash[:notice] = 'You are now logged out'
    redirect_to root_path
  end
end

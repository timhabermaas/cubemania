class LoginsController < ApplicationController
  login :only => :destroy
  logout :only => [:show, :create]

  def show
    @login = Login.new
  end

  def create
    @login = Login.new params[:login]
    if self.user = @login.validate
      flash[:notice] = "Hello #{user.name}, you are now logged in"
      redirect_back root_path
    else
      render :action => 'show'
    end
  end

  def destroy
    self.user = nil
    flash[:notice] = 'You are now logged out'
    redirect_to root_path
  end
end
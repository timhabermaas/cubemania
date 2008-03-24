class SessionsController < ApplicationController
  login :only => :destroy
  logout :only => [:new, :create]

  def index
    redirect_to login_path
  end

  def new
    @session = Session.new
  end

  def create
    @session = Session.new params[:session]
    if self.user = @session.validate
      flash[:notice] = "Hello #{user.name}, you are now logged in"
      redirect_to root_path
    else
      render :action => 'new'
    end
  end

  def destroy
    self.user = nil
    flash[:notice] = 'You are now logged out'
    redirect_to root_path
  end
end
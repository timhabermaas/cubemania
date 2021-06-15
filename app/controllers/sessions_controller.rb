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
      flash[:notice] = "Hello #{current_user.name}, you are now logged in."
      redirect_back current_user
    else
      render :new
    end
  end

  def show
    if self.current_user
      payload = { :user_id => current_user.id, :exp => Time.now.to_i + 30 * 3600 }
      render :json => { :user_id => self.current_user, :token => JWT.encode(payload, ENV.fetch("HMAC_SECRET"), "HS256")  }
    else
      render :json => { :error => "unauthorized" }, :status => :not_found
    end
  end

  def destroy
    self.current_user = nil
    flash[:notice] = "You are now logged out."
    redirect_to root_path
  end
end

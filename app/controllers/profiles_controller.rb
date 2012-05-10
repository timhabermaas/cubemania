class ProfilesController < ApplicationController
  skip_load_and_authorize_resource
  skip_login :only => [:new, :create]
  logout :only => [:new, :create]

  def new
    @user = User.new
  end

  def create
    @user = User.new params[:user]

    if @user.save
      UserMailer.welcome(@user).deliver
      flash[:notice] = "Hello #{@user.name}, you are now registered."
      self.current_user = @user
      redirect_to @user
    else
      render :new
    end
  end

  def edit
    @user = object
  end

  def update
    @user = object

    if @user.update_attributes params[:user], :as => current_user.role.to_sym
      redirect_to @user, :notice => "Profile successfully updated."
    else
      render :edit
    end
  end

  def destroy
    @user = object

    if @user.destroy
      if self.current_user == @user
        self.current_user = nil
      end

      redirect_to root_path, :notice => "Your profile has been successfully deleted."
    else
      redirect_to root_path, :notice => "We couldn't delete your profile."
    end
  end

private
  def object(options = nil)
    User.find params[:id], options
  end
end

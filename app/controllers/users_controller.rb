class UsersController < ApplicationController
  respond_to :html
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

  def block
    @user = object
    @user.block!
  end

  def follow
    user = User.find params[:id]
    if current_user.follow!(user)
      redirect_to user, :notice => "You are now following #{user.name}."
    else
      redirect_to user
    end
  end

  def unfollow
    user = User.find params[:id]
    if current_user.unfollow!(user)
      redirect_to user, :notice => "You are no longer following #{user.name}."
    else
      redirect_to user
    end
  end

  def index
    @users = User.order("singles_count desc").paginate(:page => params[:page], :per_page => 200)
    if params[:q]
      @users = @users.where('lower(name) LIKE ?', "%#{params[:q].downcase}%")
    end
    @max_singles_count = User.order("singles_count desc").first.singles_count
    respond_to do |format|
      format.html
      format.js
    end
  end

  def show
    @user = object
    @records = @user.records
    @activity = @user.singles.grouped(by: :day, time_zone: @user.time_zone).where(:created_at => 1.year.ago..Time.zone.now)
    if logged_in? and current_user != @user
      @own_records = current_user.records.grouped_by_puzzle_and_amount
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
  def object
    @user ||= User.find params[:id]
  end
end

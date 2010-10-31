class UsersController < ApplicationController
  skip_login :only => [:new, :create]
  logout :only => [:new, :create]
  permit :self, :only => [:edit, :update, :destroy]
  #protect [:role, :sponsor, :ignored], :but => :admin, :only => [:create, :update]

  def index
    @max_singles_count = User.max_singles_count
    @users =
      if params[:search]
        User.order('singles_count desc').where('name LIKE ?', "%#{params[:search] || ''}%")
      else
        User.order('singles_count desc').paginate(:page => params[:page], :per_page => 100)
      end
  end

  def show
    @user = object
    grouped_by_puzzles = @user.records.group_by { |r| r.puzzle }
    @records = grouped_by_puzzles.merge(grouped_by_puzzles) { |k, v| v = v.group_by { |r| r.amount }; v.merge(v) { |k, v| v.try(:first) } }
  end

  def object(options = nil)
    User.find params[:id], options
  end

  def create
    @user = User.new params[:user]
    if @user.save
      flash[:notice] = "Hello #{@user.name}, you are now registered"
      self.current_user = @user
      redirect_back @user
    else
      render :new
    end
  end

  def update
    @user = User.find params[:id]
    if @user.update_attributes params[:user]
      flash[:notice] = "Successfully updated"
      redirect_to user_path
    else
      render :_form
    end
  end

  def destroy
    @user = User.find params[:id]
    @user.destroy

    if self.current_user == @user
      self.current_user = nil
    end

    redirect_to root_path
  end
end
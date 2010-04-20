class UsersController < ApplicationController
  skip_login :only => [:new, :create]
  logout :only => [:new, :create]
  permit :self, :only => [:edit, :update, :destroy]
  #protect [:role, :sponsor, :ignored], :but => :admin, :only => [:create, :update]

  def index
    if params[:search]
      @users = User.paginate :page => params[:page], :per_page => 50, :order => 'points DESC', :conditions => ['name LIKE ?', "%#{params[:search]}%"]
    else
      @users = User.paginate :page => params[:page], :per_page => 50, :order => 'points DESC'
    end
  end

  def show
    @user = object
    single_records, average_records = @user.singles.records, @user.averages.records
    @records = (0...single_records.size).map do |i|
      unless average_records[i].nil? or single_records[i].puzzle_id == average_records[i].puzzle_id
        average_records.insert i, nil
      end
      { :single => single_records[i], :average => average_records[i] }
    end
    @participances = @user.participances
    @matches = Match.for(@user).finished.recent.all(:include => [{:puzzle => :kind}, :user, :opponent])
  end

  def object
    User.find params[:id]
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
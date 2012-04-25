class UsersController < ApplicationController
  respond_to :html, :json

  skip_login :only => [:new, :create]
  logout :only => [:new, :create]
  #protect [:role, :sponsor, :ignored], :but => :admin, :only => [:create, :update]

  def index
    @users = User.order('singles_count desc').paginate(:page => params[:page], :per_page => 100)
    @users = @users.where('lower(name) LIKE ?', "%#{params[:q].downcase}%") if params[:q]

    respond_with @users
  end

  def show
    @user = object
    #grouped_by_puzzles = @user.records.group_by { |r| r.puzzle }
    #@records = grouped_by_puzzles.merge(grouped_by_puzzles) { |k, v| v = v.group_by { |r| r.amount }; v.merge(v) { |k, v| v.try(:first) } }
    respond_to do |format|
      format.html
      # TODO better use record.puzzle_name (model)
      format.json { render :json => @user.to_json(:include => {:records => {:include => {:puzzle => {:include => :kind}}}}) }
    end
  end

  def object(options = nil)
    User.find params[:id], options
  end

  def create
    @user = User.new params[:user]
    if @user.save
      UserMailer.welcome(@user).deliver
      flash[:notice] = "Hello #{@user.name}, you are now registered."
      self.current_user = @user
      redirect_back @user
    else
      render :new
    end
  end

  def update
    @user = User.find params[:id]
    if @user.update_attributes params[:user], :as => current_user.role.to_sym
      flash[:notice] = "Successfully updated"
      redirect_to @user
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
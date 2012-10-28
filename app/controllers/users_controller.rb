class UsersController < ApplicationController
  respond_to :html

  def index
    @users = User.order("singles_count desc").paginate(:page => params[:page], :per_page => 200)
    if params[:q]
      @users = @users.where('lower(name) LIKE ?', "%#{params[:q].downcase}%")
    end
    @max_singles_count = User.order("singles_count desc").first.singles_count
  end

  def show
    @user = object
  end

private
  def object
    @user ||= User.find params[:id]
  end
end

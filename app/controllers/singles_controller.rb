class SinglesController < ApplicationController
  login :only => :index

  def index
    @user = User.find params[:user_id]
    @singles = @user.singles.for params[:puzzle_id]
  end
end
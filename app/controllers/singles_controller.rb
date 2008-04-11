class SinglesController < ApplicationController
  login

  def index
    @user = User.find params[:user_id]
    @singles = @user.singles.for params[:puzzle]
    respond_to &:xml
  end
end
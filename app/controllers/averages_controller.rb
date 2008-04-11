class AveragesController < ApplicationController
  login

  def index
    @user = User.find params[:user_id]
    @averages = @user.averages.for params[:puzzle]
    respond_to &:xml
  end
end
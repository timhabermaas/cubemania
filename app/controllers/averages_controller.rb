class AveragesController < ApplicationController
  login :only => :index

  def index
    @user = User.find params[:user_id]
    @averages = @user.averages.for params[:puzzle_id]
    respond_to &:xml
  end
end
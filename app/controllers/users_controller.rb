class UsersController < ApplicationController
  def show
    @user = object
  end

private
  def object
    @user ||= User.find params[:id]
  end
end

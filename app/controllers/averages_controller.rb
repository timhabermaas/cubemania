class AveragesController < ApplicationController
  login

  def index
    @averages = User.find(params[:user_id]).averages.for params[:puzzle]
    respond_to do |format|
      format.xml
    end
  end
end
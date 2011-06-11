class SinglesController < ApplicationController
  respond_to :html, :json

  def index
    user = User.find params[:user_id]
    @singles = user.singles.for(params[:puzzle_id]).paginate(:page => params[:page], :per_page => 100)
    respond_to do |format|
      format.html
      format.json { render :json => @singles.to_json(:only => [:id, :time, :created_at, :penalty]) }
    end
  end
end

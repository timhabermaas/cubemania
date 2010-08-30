class SinglesController < ApplicationController
  respond_to :html, :json

  def index
    user = User.find params[:user_id]
    @singles = user.singles.for(params[:puzzle_id])
    respond_with(@singles)
  end
end

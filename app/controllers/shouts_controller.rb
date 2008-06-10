class ShoutsController < ApplicationController
  def create
    shout = current_user.shouts.build params[:shout].merge :competition_id => params[:competition_id]
    shout.save
    redirect_to puzzle_competition_path(params[:puzzle_id], params[:competition_id])
  end
end

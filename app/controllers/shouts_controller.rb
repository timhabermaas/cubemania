class ShoutsController < ApplicationController
  permit :moderator, :only => :destroy

  def create
    shout = current_user.shouts.build params[:shout].merge :competition_id => params[:competition_id]
    shout.save
    redirect_to puzzle_competition_path(params[:puzzle_id], params[:competition_id])
  end

  def destroy
    object.destroy
    redirect_to puzzle_competition_path(params[:puzzle_id], params[:competition_id])
  end

  private
    def object
      @shout ||= Shout.find params[:id]
    end
end
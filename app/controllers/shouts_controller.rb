class ShoutsController < ApplicationController
  permit :moderator, :only => :destroy

  def create
    @puzzle = Puzzle.find params[:puzzle_id]
    @competition = @puzzle.competitions.find params[:competition_id]
    @shout = current_user.shouts.build params[:shout].merge :competition_id => @competition
    @shout.save
  end

  def destroy
    object.destroy
    redirect_to puzzle_competition_path params[:puzzle_id], params[:competition_id], :anchor => 'comments'
  end

  private
    def object
      @shout ||= Shout.find params[:id]
    end
end
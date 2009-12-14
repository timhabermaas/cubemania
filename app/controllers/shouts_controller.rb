class ShoutsController < ApplicationController
  permit :moderator, :only => :destroy

  def create
    @puzzle = Puzzle.find params[:puzzle_id]
    @parent = @puzzle.competitions.find params[:competition_id]
    @shout = current_user.shouts.build(params[:shout])
    @shout.save
  end

  def destroy
    object.destroy
    redirect_to puzzle_competition_path(params[:puzzle_id], params[:competition_id], :anchor => 'shouts')
  end

  private
    def object
      @shout ||= Shout.find params[:id]
    end
end
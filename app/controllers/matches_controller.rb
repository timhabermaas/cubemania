class MatchesController < ApplicationController
  
  def index
    if logged_in?
      @finished_matches = current_user.home_matches.finished.recent + current_user.guest_matches.finished.recent
      @challenged_matches = current_user.guest_matches.challenged
      @awaiting_matches = current_user.home_matches.challenged
    else
      @matches = Match.finished.recent
    end
  end

  def show
    @puzzle = Puzzle.find params[:puzzle_id]
    @match = @puzzle.matches.find params[:id]
    @scrambles = @match.scrambles.collect(&:scramble)
  end

  def create
    @user = User.find params[:user_id]
    @match = current_user.home_matches.build(:puzzle_id => params[:puzzle_id], :opponent_id => params[:user_id])
    @match.save
    redirect_to puzzle_match_path(params[:puzzle_id], @match)
  end

  def new
    @user = User.find params[:user_id]
    @puzzle = Puzzle.find params[:puzzle_id]
  end
end

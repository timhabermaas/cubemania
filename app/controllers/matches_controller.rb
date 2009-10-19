class MatchesController < ApplicationController
  
  def index
    #@requests = current_user.guest_matches.unfinished
    @matches = current_user.matches#current_user.home_matches.finished + current_user.guest_matches.finished
  end

  def show
    @puzzle = Puzzle.find params[:puzzle_id]
    @match = @puzzle.matches.find params[:id]
    @scrambles = @puzzle.scrambles
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

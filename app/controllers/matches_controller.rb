class MatchesController < ApplicationController
  before_filter :show_permit, :only => :show

  def new
    @user = User.find params[:user_id]
    @puzzle = Puzzle.find params[:puzzle_id]
    @match = current_user.home_matches.build(:opponent => @user)
  end

  def index
    if logged_in?
      @finished_matches = Match.for(current_user).finished.recent
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
    if @match.save
      redirect_to puzzle_match_path(params[:puzzle_id], @match)
    else
      flash[:notice] = "You can't challenge yourself!"
      redirect_to matches_path
    end
  end

  def destroy
    if object.destroy
      flash[:notice] = 'Successfully removed!'
    end
    redirect_to matches_path
  end

private
  def object
    @match ||= parent.matches.find params[:id]
  end

  def parent
    @puzzle ||= Puzzle.find params[:puzzle_id]
  end

  def show_permit
    if not object.finished? and not logged_in?
      flash[:notice] = 'Please login or <a href="/register">register</a> to continue'
      store_location
      redirect_to login_path
    elsif not object.finished? and (object.opponent != current_user and object.user != current_user)
      flash[:notice] = 'You do not have the necessary permissions'
      redirect_to root_url
    end
  end
end

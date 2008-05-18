class CompetitionsController < ApplicationController
  def index
    @competitions = Competition.find :all, :order => 'created_at'
  end
  
  def show
    @competition = Competition.find params[:id], :include => :users
    @puzzle = Puzzle.find params[:puzzle_id]
    @users = @competition.users
    @results = @competition.averages.for @puzzle
    @participation = current_user.participations.find_by_competition_id @competition if logged_in?
  end
  
  def new
    @competition = Competition.new
  end
  
  def create
    @competition = current_user.competitions.build params[:competition]
    puzzle = Puzzle.find params[:puzzle_id]
    if @competition.save
      redirect_to kind_puzzle_competition_path(params[:kind_id], params[:puzzle_id], @competition)
    end
  end
end

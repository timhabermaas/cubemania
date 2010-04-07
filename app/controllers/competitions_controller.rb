class CompetitionsController < ApplicationController
  permit :owner, :only => [:update, :destroy]
  #protect :sticky, :but => :admin, :only => [:create, :update]

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    params[:repeat] ||= 'all'
    if params[:repeat] == 'all'
      @competitions = @puzzle.competitions.includes(:user).
                      order('sticky desc, averages_count desc, created_at desc').
                      paginate(:page => params[:page], :per_page => 10)
    else
      @competitions = @puzzle.competitions.includes(:user).where(:repeat => params[:repeat]).
                      order('sticky desc, averages_count desc, created_at desc').
                      paginate(:page => params[:page], :per_page => 10)
    end
  end
  
  def create
    @puzzle = Puzzle.find params[:puzzle_id]
    @competition = current_user.competitions.build params[:competition]
    @competition.puzzle_id = params[:puzzle_id]
    @competition.save
  end
  
  def update
    @puzzle = Puzzle.find params[:puzzle_id]
    @competition = object
    @competition.update_attributes params[:competition]
  end
  
  def show
    @puzzle = Puzzle.find params[:puzzle_id]
    @competition = Competition.find params[:id]
    if params[:date].nil?
      time = Time.now.utc
    else
      time = Time.parse params[:date]
      time = Time.utc time.year, time.month, time.day
    end
    @date = time
    unless @competition.old? @date
      scrambles = @competition.scrambles.for @competition, @date
      if scrambles.empty?
        @scrambles = @competition.create_scrambles
      else
        @scrambles = scrambles.map(&:scramble)
      end
    end
    @shouts = @competition.shouts.for @competition, @date
    @parent = @competition
  end
  
  def object
    Competition.find params[:id]
  end
end
class CompetitionsController < ApplicationController
  before_filter :load_puzzle

  def index
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
    @competition = current_user.competitions.build params[:competition]
    @competition.puzzle = @puzzle
    if @competition.save
      redirect_to [@puzzle, @competition], :notice => "Successfully created competition."
    else
      redirect_to puzzle_competitions_path(@puzzle) # TODO open panel again
    end
  end

  def update
    @puzzle = Puzzle.find params[:puzzle_id]
    @competition = object
    @competition.update_attributes params[:competition]
  end

  def show
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
    @results = @competition.averages.for @competition, @date
  end

  def compete
    @competition = Competition.find params[:id]
    @average = current_user.averages.build params[:average].merge(:puzzle_id => @puzzle.id,
                                                                  :competition_id => @competition.id)

    if @average.save
      redirect_to [@puzzle, @competition]
    else
      redirect_to [@puzzle, @competition], :notice => "Get out of here!"
    end
  end

  def object
    Competition.find params[:id]
  end

  private
  def load_puzzle
    @puzzle = Puzzle.find params[:puzzle_id]
  end
end
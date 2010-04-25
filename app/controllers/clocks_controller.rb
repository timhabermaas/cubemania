class ClocksController < ApplicationController
  login :except => []
  before_filter :facebook_required, :only => :tweet

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    @scrambles = @puzzle.scrambles
  end

  def create
    @average = current_user.averages.build params[:average].merge(:puzzle_id => params[:puzzle_id],
                                                                  :competition_id => params[:competition_id],
                                                                  :match_id => params[:match_id])
    @average.singles = params[:singles].map { |index, single| current_user.singles.build single.merge(:puzzle_id => params[:puzzle_id],
                                                                                                      :competition_id => params[:competition_id],
                                                                                                      :match_id => params[:match_id],
                                                                                                      :position => index) }

    if @average.save
      @puzzle = @average.puzzle
      @scrambles = @puzzle.scrambles
    end
  end

  def tweet
    @puzzle = Puzzle.find params[:puzzle_id]
    @average = current_user.averages.find params[:id]
    logger.info "[Facebook] #{render_to_string('tweet.text', :layout => false)}"
    if Rails.env.production?
      facebook_access_token.post("/me/feed", :picture => @puzzle.image.url(:facebook),
                                             :name => @puzzle.name + ' ' + @puzzle.kind.name,
                                             :description => 'Keep track of your times and join Cubemania',
                                             :link => user_url(@average.user_id, :host => "cubemania.org"),
                                             :message => render_to_string('tweet.text', :layout => false))
    end
    respond_to do |format|
      format.js
    end
    # rescue not authorized ==> render tweet.failure.js
  end
end
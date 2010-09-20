class TimesController < ApplicationController
  login :except => []
  before_filter :facebook_required, :only => :tweet

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    @scramble = @puzzle.scramble
    @singles = current_user.singles.for(params[:puzzle_id]).paginate :page => params[:page], :per_page => 50
  end

  def create
    @puzzle = Puzzle.find params[:puzzle_id]
    @scramble = @puzzle.scramble
    @single = current_user.singles.build(params[:single].merge!(:puzzle_id => @puzzle.id))
    if @single.save
      respond_to do |format|
        format.html { redirect_to puzzle_times_path(@puzzle) }
        format.js
      end
    else
      respond_to do |format|
        format.html { redirect_to puzzle_times_path(@puzzle) }
        format.js { render 'create.failure.js' }
      end
    end
  end

  def tweet
    @puzzle = Puzzle.find params[:puzzle_id]
    @average = current_user.averages.find params[:id]
    logger.info "[Facebook] #{render_to_string('tweet.text', :layout => false)}"
    begin
      facebook_access_token.post("/me/feed", :picture => @puzzle.image.url(:facebook),
                                             :name => @puzzle.name + ' ' + @puzzle.kind.name,
                                             :description => 'Keep track of your times and join Cubemania',
                                             :link => user_url(@average.user_id, :host => "cubemania.org"),
                                             :message => render_to_string('tweet.text', :layout => false))# if Rails.env.production?
      respond_to do |format|
        format.js
      end
    rescue OAuth2::AccessDenied
      respond_to do |format|
        format.js { render 'tweet.failure.js' }
      end
    end
  end

  def destroy
    @puzzle = Puzzle.find params[:puzzle_id]
    @single = current_user.singles.find params[:id]
    @single.destroy
  end
end
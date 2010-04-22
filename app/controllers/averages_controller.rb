class AveragesController < ApplicationController
  login :except => []
  permit :owner, :only => :destroy

  def index
    @user = User.find params[:user_id]
    @puzzle = Puzzle.find params[:puzzle_id]
    @averages = @user.averages.where(:puzzle_id => params[:puzzle_id]).order('created_at desc').includes(:singles)

    respond_to do |format|
      format.html
      format.xml
      format.json
      format.csv do
        response.headers['Content-Type'] = 'text/csv; charset=iso-8859-1; header=present'
        response.headers['Content-Disposition'] = 'attachment; filename="' + @puzzle.kind.name + '_' + @puzzle.name + '.csv"'
      end
    end
  end

  # should be js request from clocks (belongs to ClocksController?)
  def tweet
    @puzzle = Puzzle.find params[:puzzle_id]
    @average = current_user.averages.find params[:id]
    logger.info "[Facebook] #{render_to_string('tweet.text')}"
    if Rails.env.production?
      facebook_client.post("/me/feed", :message => render_to_string('tweet.text'))
    end
    response.content_type = nil
    redirect_back root_path
  end

  def show
    @user = current_user
    @puzzle = Puzzle.find params[:puzzle_id]
    object :include => :singles
  end

  def destroy
    if object.destroy
      flash[:notice] = 'Successfully removed!'
    else
      flash[:notice] = "You can't remove this average, because it's part of a match"
    end
    redirect_to user_puzzle_averages_path
  end

  private
    def object(options = nil)
      @average ||= Average.find params[:id], options
    end
end
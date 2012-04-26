class RecordsController < ApplicationController
  def index
    @puzzle = Puzzle.find params[:puzzle_id]

    @records =
    if params[:user_id]
      user = User.find params[:user_id]
      user.records.where(:puzzle_id => @puzzle.id)
    else
      params[:type] ||= "avg5"

      if params[:type] == "single"
        @puzzle.records.amount(1)
      elsif params[:type] == "avg12"
        @puzzle.records.amount(12)
      else
        @puzzle.records.amount(5)
      end.paginate(:page => params[:page], :per_page => 50)
    end

    respond_to do |format|
      format.html
      format.json
    end
  end

  def show
    @puzzle = Puzzle.find params[:puzzle_id]
    @record = @puzzle.records.find params[:id], :include => [:singles]
    @user = @record.user
    @singles = @record.singles.ordered
  end

  def share
    @record = current_user.records.find params[:id]
    token = current_user.authorizations.find_by_provider("facebook").try(:token)
    if token
      begin
        me = FbGraph::User.me(token).fetch
        presenter = RecordPresenter.new(@record)
        me.feed! :name => me.first_name + " has a new " + presenter.record_type + " Record: " + presenter.human_time,
                 :picture => @record.puzzle.combined_url,
                 :link => puzzle_record_url(@record.puzzle, @record),
                 :description => presenter.singles_as_text
        redirect_to puzzle_timers_path(@record.puzzle), :notice => "Successfully shared."
      rescue FbGraph::InvalidToken
        redirect_to "/auth/facebook"
      end
    else
      redirect_to "/auth/facebook"
    end
  end
end

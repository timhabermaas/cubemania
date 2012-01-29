class RecordsController < ApplicationController
  def index
    params[:type] ||= "avg5"

    @puzzle = Puzzle.find params[:puzzle_id]
    @records =
      if params[:type] == "single"
        @puzzle.records.amount(1)
      elsif params[:type] == "avg12"
        @puzzle.records.amount(12)
      else
        @puzzle.records.amount(5)
      end.paginate(:page => params[:page], :per_page => 50)
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
        me = FbGraph::User.me(token)
        me.feed! :message => "test",
                 :name => RecordPresenter.new(@record).record_type,
                 :picture => @record.puzzle.combined_url,
                 :link => puzzle_record_url(@record.puzzle, @record)
        redirect_to root_path, :notice => "Successfully shared."
      rescue FbGraph::InvalidToken
        redirect_to "/auth/facebook"
      end
    else
      redirect_to "/auth/facebook"
    end
  end
end

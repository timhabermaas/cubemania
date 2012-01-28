class SharesController < ApplicationController
  skip_load_and_authorize_resource

  def create
    @record = current_user.records.find params[:record_id]
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

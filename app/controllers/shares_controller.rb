class SharesController < ApplicationController
  skip_load_and_authorize_resource

  def create
    @record = current_user.records.find params[:record_id]
    me = FbGraph::User.me(current_user.authorizations.find_by_provider("facebook").token)
    me.feed! :message => "test",
             :name => RecordPresenter.new(@record).record_type,
             :picture => @record.puzzle.combined_url,
             :link => puzzle_record_url(@record.puzzle, @record)
    redirect_to root_path
  end
end

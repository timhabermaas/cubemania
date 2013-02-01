class RecordsController < ApplicationController
  before_filter :set_type_default, :only => :index

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    amount = # TODO move into extra class (mapping of int to description)
      case params[:type]
        when "single"
          1
        when "avg12"
          12
        else
          5
      end

    @records = @puzzle.records.amount(amount).paginate(:page => params[:page], :per_page => 50)
  end

  def show
    @user = User.find params[:user_id]
    @record = @user.records.find params[:id], :include => [:singles]
    @puzzle = @record.puzzle
    @singles = @record.singles.ordered
  end

  def share
    @record = current_user.records.find params[:id]
    post = FacebookPost.new @record

    options = {
                :app_id => ENV["FACEBOOK_APP_ID"],
                :link => user_record_url(@record.user, @record),
                :picture => @record.puzzle.combined_url,
                :name => post.title,
                :caption => post.caption,
                :description => post.body,
                :redirect_uri => user_record_url(@record.user, @record)
              }

    url = "http://www.facebook.com/dialog/feed?" + URI.encode(options.collect { |k, v| "#{k}=#{v}"}.join("&"))
    redirect_to url
  end

private
  def set_type_default
    params[:type] = params[:type] || "avg5"
  end
end

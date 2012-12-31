class RecordsController < ApplicationController
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
    @puzzle = Puzzle.find params[:puzzle_id]
    @record = @puzzle.records.find params[:id], :include => [:singles]
    @user = @record.user
    @singles = @record.singles.ordered
  end

  def share
    @record = current_user.records.find params[:id]
    presenter = RecordPresenter.new(@record)

    options = { :app_id => ENV["FACEBOOK_APP_ID"],
                :link => puzzle_record_url(@record.puzzle, @record),
                :picture => @record.puzzle.combined_url,
                :name => "#{current_user.name.capitalize} has a new #{presenter.full_puzzle_name} " + presenter.record_type + " record: " + presenter.human_time,
                :caption => "Keep track of your times and join Cubemania!",
                :description => presenter.singles_as_text,
                :redirect_uri => puzzle_record_url(@record) }

    url = "http://www.facebook.com/dialog/feed?" + URI.encode(options.collect { |k, v| "#{k}=#{v}"}.join("&"))
    redirect_to url
  end
end

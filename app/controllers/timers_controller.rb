class TimersController < ApplicationController
  login :except => []
  skip_load_and_authorize_resource
  load_and_authorize_resource :class => Single
  before_filter :load_puzzle

  def index
    @scramble = @puzzle.scramble
    @singles = current_user.singles.for(@puzzle).paginate :page => params[:page], :per_page => 12
    fetch_records
  end

  def create
    @scramble = @puzzle.scramble
    @single = current_user.singles.build(params[:single].merge(:puzzle_id => @puzzle.id))
    if @single.save
      UpdateRecentRecords.for(current_user, @puzzle)
      fetch_records
      set_flash
      respond_to do |format|
        format.html { redirect_to puzzle_timers_path(@puzzle) }
        format.js
      end
    else
      respond_to do |format|
        format.html { redirect_to puzzle_timers_path(@puzzle) }
        format.js { render 'create.failure.js' }
      end
    end
  end

  def dnf
    @single = current_user.singles.find params[:id]
    @single.toggle_dnf!
    enqueue_record_job current_user, @puzzle
    respond_to do |format|
      format.html { redirect_to puzzle_timers_path(@puzzle) }
      format.js { render :penalty }
    end
  end

  def plus2
    @single = current_user.singles.find params[:id]
    @single.toggle_plus2!
    enqueue_record_job current_user, @puzzle
    respond_to do |format|
      format.html { redirect_to puzzle_timers_path(@puzzle) }
      format.js { render :penalty }
    end
  end

  def destroy
    @single = current_user.singles.find params[:id]
    if @single.destroy
      enqueue_record_job current_user, @puzzle
    else
      flash[:alert] = "Couldn't remove single, because it belongs to a competition."
    end

    respond_to do |format|
      format.html { redirect_to puzzle_timers_path(@puzzle) }
      format.js
    end
  end

  def more
    @singles = current_user.singles.for(@puzzle).paginate :page => params[:page], :per_page => 12
    respond_to do |format|
      format.html { redirect_to puzzle_timers_path(@puzzle, :page => params[:page]) }
      format.js
    end
  end

  def chart
    user = User.find_by_id params[:user_id] || current_user
    @singles = user.singles.for(@puzzle).recent(200).reverse
    @singles = @singles.map { |s| { :id => s.id, :y => s.time } }
    respond_to do |format|
      format.json { render :json => @singles.to_json }
    end
  end

private
  def fetch_records
    @records = { 1 => current_user.records.for(@puzzle, 1),
                 5 => current_user.records.for(@puzzle, 5),
                12 => current_user.records.for(@puzzle, 12) }
  end

  def load_puzzle
    @puzzle = Puzzle.find params[:puzzle_id]
  end

  def set_flash
    messages = []
    if @records[1] && @records[1].singles.include?(@single)
      messages << "You have a new single record: <strong>#{view_context.ft(@records[1].time)}</strong>! #{view_context.link_to 'Share', puzzle_record_path(@puzzle, @records[1])}".html_safe
    end
    if @records[5] && @records[5].singles.include?(@single)
      messages << "You have a new average of 5 record: <strong>#{view_context.ft(@records[5].time)}</strong>! #{view_context.link_to 'Share', puzzle_record_path(@puzzle, @records[5])}".html_safe
    end
    if @records[12] && @records[12].singles.include?(@single)
      messages << "You have a new average of 12 record: <strong>#{view_context.ft(@records[12].time)}</strong>! #{view_context.link_to 'Share', puzzle_record_path(@puzzle, @records[12])}".html_safe
    end
    flash[:notice] = messages.join("<br />") unless messages.empty?
  end

  def enqueue_record_job(user, puzzle)
    job = RecordCalculationJob.new(user.id, puzzle.id)
    Delayed::Job.enqueue job unless job.exists?
  end
end
class TimersController < ApplicationController
  login :except => []
  skip_load_and_authorize_resource
  load_and_authorize_resource :class => Single
  before_filter :load_puzzle

  def index
    @scramble = @puzzle.scramble
    @singles = current_user.singles.for(@puzzle).last_24_hours
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
    @singles = current_user.singles.for(@puzzle).limit(12)
    if params[:after]
      last = DateTime.parse params[:after]
      @singles = @singles.where "singles.created_at < ?", last
    end
    respond_to do |format|
      format.html { redirect_to puzzle_timers_path(@puzzle, :page => params[:page]) }
      format.js
    end
  end

  def chart
    user = User.find_by_id params[:user_id] || current_user
    @singles = user.singles.for(@puzzle).recent(150).reverse
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
    messages = [1, 5, 12].map do |amount|
      if @records[amount] && @records[amount].singles.include?(@single)
        RecordPresenter.new(@records[amount]).flash_message + " " +
          view_context.link_to('Share it', puzzle_record_path(@puzzle, @records[amount]))
      end
    end.compact
    flash[:notice] = messages.join("<br />").html_safe unless messages.empty?
  end

  def enqueue_record_job(user, puzzle)
    job = RecordCalculationJob.new(user.id, puzzle.id)
    Delayed::Job.enqueue job unless job.exists?
  end
end
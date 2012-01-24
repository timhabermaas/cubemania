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
      UpdateRecords.for(current_user, @puzzle)
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
    respond_to do |format|
      format.html { redirect_to puzzle_timers_path(@puzzle) }
      format.js { render :penalty }
    end
  end

  def plus2
    @single = current_user.singles.find params[:id]
    @single.toggle_plus2!
    respond_to do |format|
      format.html { redirect_to puzzle_timers_path(@puzzle) }
      format.js { render :penalty }
    end
  end

  def destroy
    @single = current_user.singles.find params[:id]
    @single.destroy
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
    if @records[1] && @records[1].singles.include?(@single)
      flash[:notice] = "You have a new single record with #{view_context.ft(@single.time)}! #{view_context.link_to 'Share', root_path}".html_safe
    end
  end
end
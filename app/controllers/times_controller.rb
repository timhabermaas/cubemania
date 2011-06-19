class TimesController < ApplicationController
  login :except => []
  skip_load_and_authorize_resource
  load_and_authorize_resource :class => Single

  before_filter :fetch_records, :except => :create

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    @scramble = @puzzle.scramble
    @singles = current_user.singles.for(params[:puzzle_id]).paginate :page => params[:page], :per_page => 100
  end

  def create
    @puzzle = Puzzle.find params[:puzzle_id]
    @scramble = @puzzle.scramble
    @single = current_user.singles.build(params[:single].merge!(:puzzle_id => @puzzle.id))
    if @single.save
      fetch_records
      set_flash
      respond_to do |format|
        format.html { redirect_to puzzle_times_path(@puzzle) }
        format.js
      end
    else
      respond_to do |format|
        format.html { redirect_to puzzle_times_path(@puzzle) }
        format.js { render 'create.failure.js' }
      end
    end
  end

  def dnf
    @puzzle = Puzzle.find params[:puzzle_id]
    @single = current_user.singles.find params[:id]
    @single.toggle_dnf!
    respond_to do |format|
      format.html { redirect_to puzzle_times_path(@puzzle) }
      format.js { render :penalty }
    end
  end

  def plus2
    @puzzle = Puzzle.find params[:puzzle_id]
    @single = current_user.singles.find params[:id]
    @single.toggle_plus2!
    respond_to do |format|
      format.html { redirect_to puzzle_times_path(@puzzle) }
      format.js { render :penalty }
    end
  end

  def destroy
    @puzzle = Puzzle.find params[:puzzle_id]
    @single = current_user.singles.find params[:id]
    @single.destroy
    respond_to do |format|
      format.html { redirect_to puzzle_times_path(@puzzle) }
      format.js
    end
  end

private
  def fetch_records
    @records = { 1 => current_user.records.for(params[:puzzle_id], 1),
                 5 => current_user.records.for(params[:puzzle_id], 5),
                12 => current_user.records.for(params[:puzzle_id], 12) }
  end

  # TODO deliver only time and format (1, 5, 12). client should create a proper message out of it
  def set_flash
    times = []
    formats = []
    [1, 5, 12].each do |n|
      if @records[n].try(:has_single?, @single)
        times << @records[n].time
        formats << n
      end
    end
    flash[:notice] = times.join "@"
    flash[:title] = formats.join "@"
  end
end
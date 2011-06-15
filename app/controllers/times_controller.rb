class TimesController < ApplicationController
  login :except => []
  skip_load_and_authorize_resource
  load_and_authorize_resource :class => Single

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    @scramble = @puzzle.scramble
    @records = current_user.records.for(@puzzle.id, 1, 5, 12)
    @singles = current_user.singles.for(params[:puzzle_id]).paginate :page => params[:page], :per_page => 100
  end

  def create
    @puzzle = Puzzle.find params[:puzzle_id]
    @scramble = @puzzle.scramble
    @single = current_user.singles.build(params[:single].merge!(:puzzle_id => @puzzle.id))
    if @single.save
      @records = current_user.records.for(@puzzle.id, 1, 5, 12)
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
    @records = current_user.records.for(@puzzle.id, 1, 5, 12)
    respond_to do |format|
      format.html { redirect_to puzzle_times_path(@puzzle) }
      format.js { render :penalty }
    end
  end

  def plus2
    @puzzle = Puzzle.find params[:puzzle_id]
    @single = current_user.singles.find params[:id]
    @single.toggle_plus2!
    @records = current_user.records.for(@puzzle.id, 1, 5, 12)
    respond_to do |format|
      format.html { redirect_to puzzle_times_path(@puzzle) }
      format.js { render :penalty }
    end
  end

  def destroy
    @puzzle = Puzzle.find params[:puzzle_id]
    @single = current_user.singles.find params[:id]
    @single.destroy
    @records = current_user.records.for(@puzzle.id, 1, 5, 12)
    respond_to do |format|
      format.html { redirect_to puzzle_times_path(@puzzle) }
      format.js
    end
  end
end
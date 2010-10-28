class TimesController < ApplicationController
  login :except => []

  def index
    @puzzle = Puzzle.find params[:puzzle_id]
    @scramble = @puzzle.scramble
    @avg1 = current_user.records.for(params[:puzzle_id], 1)
    @avg5 = current_user.records.for(params[:puzzle_id], 5)
    @avg12 = current_user.records.for(params[:puzzle_id], 12)
    @singles = current_user.singles.for(params[:puzzle_id]).paginate :page => params[:page], :per_page => 50
  end

  def create
    @puzzle = Puzzle.find params[:puzzle_id]
    @scramble = @puzzle.scramble
    @single = current_user.singles.build(params[:single].merge!(:puzzle_id => @puzzle.id))
    if @single.save
      @avg1 = current_user.records.for(params[:puzzle_id], 1)
      @avg5 = current_user.records.for(params[:puzzle_id], 5)
      @avg12 = current_user.records.for(params[:puzzle_id], 12)
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
    @avg1 = current_user.records.for(params[:puzzle_id], 1)
    @avg5 = current_user.records.for(params[:puzzle_id], 5)
    @avg12 = current_user.records.for(params[:puzzle_id], 12)
  end

  def destroy
    @puzzle = Puzzle.find params[:puzzle_id]
    @single = current_user.singles.find params[:id]
    @single.destroy
    @avg1 = current_user.records.for(params[:puzzle_id], 1)
    @avg5 = current_user.records.for(params[:puzzle_id], 5)
    @avg12 = current_user.records.for(params[:puzzle_id], 12)
  end
end
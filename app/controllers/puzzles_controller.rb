class PuzzlesController < ApplicationController
  def index
    @puzzles = Puzzle.order('kind_id, name').includes(:kind)
  end

  def new
    @puzzle = Puzzle.new
  end

  def create
    @puzzle = Puzzle.new params[:puzzle]
    if @puzzle.save
      redirect_to puzzles_path
    else
      render :new
    end
  end

  def edit
    @puzzle = object
  end

  def update
    @puzzle = object
    if @puzzle.update_attributes params[:puzzle]
      redirect_to puzzles_path
    else
      render :edit
    end
  end

  def destroy
    object.destroy
    redirect_to puzzles_path
  end

private
  def object
    @puzzle = Puzzle.find params[:id]
  end
end
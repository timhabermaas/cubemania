class PuzzlesController < ApplicationController
  permit :admin

  def new
    @puzzle = Puzzle.new
  end

  def edit
    @puzzle = Puzzle.find params[:id]
  end

  def update
    @puzzle = Puzzle.find params[:id]
    if @puzzle.update_attributes params[:puzzle]
      redirect_to puzzles_path
    else
      render :edit
    end
  end

  def create
    @puzzle = Puzzle.new params[:puzzle]
    if @puzzle.save
      redirect_to puzzles_path
    else
      render :new
    end
  end

  def index
    @puzzles = Puzzle.order('kind_id, name').includes(:kind)
  end

  def object
    @puzzle = Puzzle.find params[:id]
  end
end
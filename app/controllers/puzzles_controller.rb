class PuzzlesController < ApplicationController
  # GET /puzzles
  # GET /puzzles.xml
  def index
    @puzzles = Puzzle.find(:all)

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @puzzles }
    end
  end

  # GET /puzzles/1
  # GET /puzzles/1.xml
  def show
    @puzzle = Puzzle.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @puzzle }
    end
  end

  # GET /puzzles/new
  # GET /puzzles/new.xml
  def new
    @puzzle = Puzzle.new

    respond_to do |format|
      format.html # new.html.erb
      format.xml  { render :xml => @puzzle }
    end
  end

  # GET /puzzles/1/edit
  def edit
    @puzzle = Puzzle.find(params[:id])
  end

  # POST /puzzles
  # POST /puzzles.xml
  def create
    @puzzle = Puzzle.new(params[:puzzle])

    respond_to do |format|
      if @puzzle.save
        flash[:notice] = 'Puzzle was successfully created.'
        format.html { redirect_to(@puzzle) }
        format.xml  { render :xml => @puzzle, :status => :created, :location => @puzzle }
      else
        format.html { render :action => "new" }
        format.xml  { render :xml => @puzzle.errors, :status => :unprocessable_entity }
      end
    end
  end

  # PUT /puzzles/1
  # PUT /puzzles/1.xml
  def update
    @puzzle = Puzzle.find(params[:id])

    respond_to do |format|
      if @puzzle.update_attributes(params[:puzzle])
        flash[:notice] = 'Puzzle was successfully updated.'
        format.html { redirect_to(@puzzle) }
        format.xml  { head :ok }
      else
        format.html { render :action => "edit" }
        format.xml  { render :xml => @puzzle.errors, :status => :unprocessable_entity }
      end
    end
  end

  # DELETE /puzzles/1
  # DELETE /puzzles/1.xml
  def destroy
    @puzzle = Puzzle.find(params[:id])
    @puzzle.destroy

    respond_to do |format|
      format.html { redirect_to(puzzles_url) }
      format.xml  { head :ok }
    end
  end
end

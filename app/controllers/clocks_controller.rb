class ClocksController < ApplicationController
  # GET /clocks
  # GET /clocks.xml
  def index
    @clocks = Clock.find(:all)

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @clocks }
    end
  end

  # GET /clocks/1
  # GET /clocks/1.xml
  def show
    @clock = Clock.find(params[:id])

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @clock }
    end
  end

  # GET /clocks/new
  # GET /clocks/new.xml
  def new
    @clock = Clock.new

    respond_to do |format|
      format.html # new.html.erb
      format.xml  { render :xml => @clock }
    end
  end

  # GET /clocks/1/edit
  def edit
    @clock = Clock.find(params[:id])
  end

  # POST /clocks
  # POST /clocks.xml
  def create
    @clock = Clock.new(params[:clock])

    respond_to do |format|
      if @clock.save
        flash[:notice] = 'Clock was successfully created.'
        format.html { redirect_to(@clock) }
        format.xml  { render :xml => @clock, :status => :created, :location => @clock }
      else
        format.html { render :action => "new" }
        format.xml  { render :xml => @clock.errors, :status => :unprocessable_entity }
      end
    end
  end

  # PUT /clocks/1
  # PUT /clocks/1.xml
  def update
    @clock = Clock.find(params[:id])

    respond_to do |format|
      if @clock.update_attributes(params[:clock])
        flash[:notice] = 'Clock was successfully updated.'
        format.html { redirect_to(@clock) }
        format.xml  { head :ok }
      else
        format.html { render :action => "edit" }
        format.xml  { render :xml => @clock.errors, :status => :unprocessable_entity }
      end
    end
  end

  # DELETE /clocks/1
  # DELETE /clocks/1.xml
  def destroy
    @clock = Clock.find(params[:id])
    @clock.destroy

    respond_to do |format|
      format.html { redirect_to(clocks_url) }
      format.xml  { head :ok }
    end
  end
end

class KindsController < ApplicationController

  def index
    @kinds = Kind.all
  end

  def new
    @kind = Kind.new
  end

  def edit
    @kind = Kind.find params[:id]
  end

  def create
    @kind = Kind.new params[:kind]
    if @kind.save
      redirect_to kinds_path
    else
      render :new
    end
  end

  def update
    @kind = Kind.find params[:id]
    if @kind.update_attributes params[:kind]
      redirect_to kinds_path
    else
      render :edit
    end
  end

  def object
    Kind.find params[:id]
  end
end
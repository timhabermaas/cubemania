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
    if object.save
      redirect_to kinds_path
    else
      render :new
    end
  end

  def update
    @kind = Kind.find params[:id]
    if object.update_attributes params[:kind]
      redirect_to kinds_path
    else
      render :edit
    end
  end

  def destroy
    object.destroy
    redirect_to kinds_path
  end

  def object
    Kind.find params[:id]
  end
end
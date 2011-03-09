class ItemsController < ApplicationController
  permit :admin

  def index
    @items = Item.all
  end

  def new
    @items = Item.new
  end

  def edit
    @item = Item.find params[:id]
  end

  def destroy
    @item = Item.find params[:id]
    @item.destroy
    redirect_to items_path
  end

  def update
    @item = Item.find params[:id]
    if @item.update_attributes params[:item]
      redirect_to items_path
    else
      render :edit
    end
  end

  def create
    @item = Item.new params[:item]
    if @item.save
      redirect_to items_path
    else
      render :new
    end
  end

  def object
    Item.find params[:id]
  end
end
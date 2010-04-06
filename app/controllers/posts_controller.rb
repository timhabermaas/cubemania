class PostsController < ApplicationController
  permit :moderator, :except => [:index, :show]
  
  def index
    @posts = Post.paginate :include => :user, :order => 'created_at desc', :page => params[:page], :per_page => 5
  end
  
  def new
    @post = Post.new
  end
  
  def edit
    @post = Post.find params[:id]
  end
  
  def show
    @post = Post.find params[:id], :include => :user
  end
  
  def create
    @post = current_user.posts.build params[:post]
    if @post.save
      redirect_to @post
    else
      render :new
    end
  end
  
  def update
    @post = current_user.posts.find params[:id]
    if @post.update_attributes params[:post]
      redirect_to @post
    else
      render :edit
    end
  end
end
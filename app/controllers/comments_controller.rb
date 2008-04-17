class CommentsController < ApplicationController
  permit :moderator, :only => :destroy

  def create
    @post = Post.find params[:post_id]
    @comment = current_user.comments.build params[:comment].merge :post_id => @post.id
    @comment.save
    respond_to &:js
  end
  
  def destroy
    @post = Post.find params[:post_id]
    @comment = @post.comments.find params[:id]
    @comment.destroy
    redirect_to post_path @post, :anchor => 'comments'
  end
end
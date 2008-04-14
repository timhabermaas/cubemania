class CommentsController < ApplicationController
  login
  
  def create
    @post = Post.find params[:post_id]
    @comment = user.comments.build params[:comment].merge :post_id => @post.id
    @comment.save
    respond_to &:js
  end
  
  def destroy
    if moderator?
      @post = Post.find params[:post_id]
      @comment = @post.comments.find params[:id]
      @comment.destroy
      redirect_to post_path @post, :anchor => 'comments'
    end
  end
end
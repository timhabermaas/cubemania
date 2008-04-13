class CommentsController < ApplicationController
  login
  
  def create
    @post = Post.find params[:post_id]
    @comment = user.comments.build params[:comment].merge :post_id => @post.id
    @comment.save
    respond_to &:js
  end
end
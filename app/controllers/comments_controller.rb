class CommentsController < ApplicationController
  def create
    @comment = parent.comments.build(params[:comment])
    @comment.user = current_user

    if @comment.save
      respond_to do |format|
        format.js
        format.html { redirect_to parent }
      end
    else
      respond_to do |format|
        format.js { render 'create.failure' }
        format.html { redirect_to parent }
      end
    end
  end

  def destroy
    object.destroy
    respond_to do |format|
      format.js
      format.html { redirect_to object.post }
    end
  end

  private
    def object
      @comment ||= parent.comments.find params[:id]
    end

    def parent
      if params[:post_id]
        @parent ||= Post.find params[:post_id]
      elsif params[:activity_id]
        @parent ||= Activity.find(params[:activity_id]).becomes(Activity)
      end
    end
    helper_method :parent
end

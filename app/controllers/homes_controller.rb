class HomesController < ApplicationController
  def show
    @announcement = Post.find :first, :include => :user, :order => 'created_at desc'
  end
end
class FeedsController < ApplicationController
  def show
    @feed = Feed.new current_user
  end
end

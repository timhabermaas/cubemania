class FeedsController < ApplicationController
  def show
    @activities = Feed.new(current_user).activities.group_by { |a| a.updated_at.beginning_of_day }
  end
end

class ActivitiesController < ApplicationController
  def index
    @activities = Feed.new(current_user).activities.group_by { |a| a.updated_at.beginning_of_day }
  end

  def show
    @activity = Activity.find params[:id]
  end
end

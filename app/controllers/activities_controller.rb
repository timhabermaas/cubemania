class ActivitiesController < ApplicationController
  def index
    @activities = Feed.new(current_user).activities.paginate(:page => params[:page], :per_page => 50)
  end

  def show
    @activity = Activity.find params[:id]
  end
end

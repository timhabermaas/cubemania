class ApplicationController < ActionController::Base
  include Authentication

  load_and_authorize_resource

  rescue_from ActiveRecord::RecordNotFound do |exception|
    render 'errors/not_found'
  end

  before_filter :set_time_zone, :store_return_to

  login :except => [:index, :show]

  protect_from_forgery

  #def rescue_action_in_public(exception)
  #  render :template => "errors/#{response_code_for_rescue(exception)}"
  #end

protected
  def facebook_required
    unless current_user.connected_to_facebook?
      flash[:notice] = "You need to connect your Cubemania profile with Facebook!"
      redirect_to new_facebook_path
    end
  end

private
  def set_time_zone
    if logged_in?
      Time.zone = current_user.time_zone
    elsif not cookies[:tz_offset].blank?
      Time.zone = ActiveSupport::TimeZone[-cookies[:tz_offset].to_i.minutes]
    end
  end

  def store_return_to
    store_location params[:return_to] unless params[:return_to].nil?
  end
end
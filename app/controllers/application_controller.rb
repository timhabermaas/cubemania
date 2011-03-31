class ApplicationController < ActionController::Base
  include Authentication

  login :except => [:index, :show]

  load_and_authorize_resource

  #protect_from_forgery

  rescue_from ActiveRecord::RecordNotFound do |exception|
    render 'errors/not_found'
  end

  rescue_from CanCan::AccessDenied do |exception|
    flash[:notice] = 'You do not have the necessary permissions'
    redirect_to root_path
  end

  before_filter :set_time_zone, :store_return_to

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
class ApplicationController < ActionController::Base
  include Authentication

  rescue_from ActiveRecord::RecordNotFound do |exception|
    render 'errors/not_found'
  end

  rescue_from OAuth2::AccessDenied do |exception|
    flash[:notice] = "Your access token doesn't seem to be valid"
    current_user.try(:update_attribute, :fb_access_token, nil)
    redirect_to new_facebook_path
  end

  before_filter :set_time_zone, :store_return_to

  login :except => [:index, :show]

  protect_from_forgery

  #def rescue_action_in_public(exception)
  #  render :template => "errors/#{response_code_for_rescue(exception)}"
  #end


=begin
  alias_method :orig_login, :login
  def login
    if request.format.json?
      authenticate_or_request_with_http_basic do |user_name, password|
        @login = Login.new :name => user_name, :password => password
        self.current_user = @login.validate
      end
    else
      orig_login
    end
  end

  alias_method :orig_permit, :permit
  def permit(role)
    if request.format.json?
      head(401) unless self.role? role
    else
      orig_permit role
    end
  end
=end
protected
  def facebook_client
    @facebook_client ||= OAuth2::Client.new(ENV['FACEBOOK_KEY'], ENV['FACEBOOK_SECRET'], :site => "https://graph.facebook.com")
  end

  def facebook_access_token
    @facebook_access_token ||= OAuth2::AccessToken.new(facebook_client, current_user.fb_access_token)
  end

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
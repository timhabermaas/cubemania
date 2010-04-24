class ApplicationController < ActionController::Base
  include Authentication

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
    @facebook_client ||= FacebookGraph::Client.new(current_user.fb_access_token)
  end

  def facebook_consumer
    @facebook_consumer ||= FacebookGraph::Consumer.new(ENV['FACEBOOK_KEY'], ENV['FACEBOOK_SECRET'],
                                                  :redirect_uri => callback_facebook_url,
                                                  :scope => "publish_stream")
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
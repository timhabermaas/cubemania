class ApplicationController < ActionController::Base
  include Authentication

  before_filter :set_time_zone, :store_return_to

  login :except => [:index, :show]

  protect_from_forgery

  #def rescue_action_in_public(exception)
  #  render :template => "errors/#{response_code_for_rescue(exception)}"
  #end
  
  
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
  
protected
  def oauth
    @oauth ||= Twitter::OAuth.new("ARtgvDlsPSj6nNfX68obg",
                                   "OeGx587uzo658jHtz8JGdpBJmyKWPMpEbIANrYqUQo",
                                   :sign_in => true)
  end
  
  def client
    oauth.authorize_from_access(current_user.twitter_token, current_user.twitter_secret)
    Twitter::Base.new(oauth)
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
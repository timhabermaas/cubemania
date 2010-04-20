class ApplicationController < ActionController::Base
  include Authentication

  helper_method :ft, :singles_as_string

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
  def twitter_client
    @twitter_client ||= TwitterOAuth::Client.new :consumer_key => ENV['TWITTER_KEY'],
                                                 :consumer_secret => ENV['TWITTER_SECRET'],
                                                 :token => current_user.twitter_token,
                                                 :secret => current_user.twitter_secret
  end

  def twitter_consumer
    @twitter_consumer ||= TwitterOAuth::Client.new :consumer_key => ENV['TWITTER_KEY'],
                                                   :consumer_secret => ENV['TWITTER_SECRET']
  end

  def last_average
    @last_average ||= Average.find_by_id(session[:last_average_id], :include => {:puzzle => :kind})
  end

  def last_average=(average)
    session[:last_average_id] = average.id
  end

  def ft(time)
    hs = (time / 10.0).round
    if hs >= 6000
      min = hs / 6000
      sec = (hs - min * 6000) / 100.0
      '%d:%05.2f' % [min, sec] + ' min' # 12.555 => "12.55"
    else
      '%.2f' % (hs.to_f / 100) + ' s'
    end
  end

  def singles_as_string(time)
    time.singles.map { |s| s.dnf? ? 'DNF' : ft(s.time) }.join ', ' if time.respond_to? :singles
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
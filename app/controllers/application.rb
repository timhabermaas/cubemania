class ApplicationController < ActionController::Base
  include Authentication
  include ExceptionNotifiable

  before_filter :update_session_expiration_date, :set_time_zone

  login :except => [:index, :show]

  protect_from_forgery

  filter_parameter_logging 'password'

  def rescue_action_in_public(exception)
    render :template => "errors/#{response_code_for_rescue(exception)}"
  end

  private
    def update_session_expiration_date
      unless ActionController::Base.session_options[:session_expires]
        ActionController::Base.session_options[:session_expires] = 1.month.from_now
      end
    end
    
    def set_time_zone
      Time.zone = current_user.time_zone if logged_in?
    end
end
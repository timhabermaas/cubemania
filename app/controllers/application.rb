class ApplicationController < ActionController::Base
  include Authentication
  include ExceptionNotifiable

  login :except => [:index, :show]

  protect_from_forgery

  filter_parameter_logging 'password'
  
  def rescue_action_in_public(exception)
    render :template => "errors/#{response_code_for_rescue(exception)}"
  end
end
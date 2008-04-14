class ApplicationController < ActionController::Base
  include Authentication
  include ExceptionNotifiable

  protect_from_forgery

  filter_parameter_logging 'password'
end
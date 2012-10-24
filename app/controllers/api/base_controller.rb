class Api::BaseController < ActionController::Base
  include Authentication

  load_and_authorize_resource

  rescue_from CanCan::AccessDenied do |exception|
    head :unauthorized
  end

  def current_ability
    @current_ability ||= ApiAbility.new(current_user)
  end
end

class Api::BaseController < ApplicationController
  def current_ability
    @current_ability ||= ApiAbility.new(current_user)
  end
end

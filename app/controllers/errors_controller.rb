class ErrorsController < ApplicationController
  login :only => []
  skip_load_and_authorize_resource

  def not_found
  end
end
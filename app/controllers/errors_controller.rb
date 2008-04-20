class ErrorsController < ApplicationController
  login :only => []

  def not_found
  end
end
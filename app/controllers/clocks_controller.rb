class ClocksController < ApplicationController
  resource_controller
  
  index.before do
    params[:puzzle] ||= '3x3x3'
    params[:kind] ||= 'speed'
  end
end
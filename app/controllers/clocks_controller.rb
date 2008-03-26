class ClocksController < ResourceController::Base
  login

  belongs_to :user

  index.before do
    params[:puzzle] ||= '3x3x3'
    params[:kind] ||= 'speed'
  end
end
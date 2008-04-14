class KindsController < ResourceController::Base
  permit :moderator
  
  [update, create].each do |action|
    action.wants.html { redirect_to kinds_path }
  end
end
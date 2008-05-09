class KindsController < ResourceController::Base
  permit :admin
  
  [update, create].each do |action|
    action.wants.html { redirect_to kinds_path }
  end
end
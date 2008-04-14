class ItemsController < ResourceController::Base
  [update, create].each do |action|
    action.wants.html { redirect_to items_path }
  end
end
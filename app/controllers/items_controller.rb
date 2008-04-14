class ItemsController < ResourceController::Base
  permit :admin

  [update, create].each do |action|
    action.wants.html { redirect_to items_path }
  end

  private
    def collection
      @collection ||= Item.all
    end
end
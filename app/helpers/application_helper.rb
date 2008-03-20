# Methods added to this helper will be available to all templates in the application.
module ApplicationHelper
  def action_label
    case params[:action].to_sym
      when :new, :create
        'Create'
      when :edit, :update
        'Update'
    end
  end
end

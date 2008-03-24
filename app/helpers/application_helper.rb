module ApplicationHelper
  def page_title
    params[:controller].titleize
  end
  
  def action_label(new = 'Create', edit = 'Update')
    case params[:action].to_sym
      when :new, :create
        new
      when :edit, :update
        edit
    end
  end
  
  def controller?(name)
    params[:controller].to_sym == name.to_sym
  end
  
  def current_item?(item)
    controller? item.url[:controller]
  end
  
  def current_puzzle?(puzzle)
    params[:puzzle] == puzzle.name
  end
  
  def current_kind?(kind)
    params[:kind] == kind.name
  end
end
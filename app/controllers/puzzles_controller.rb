class PuzzlesController < ResourceController::Base
  permit :admin
  
  [new_action, edit].each do |action|
    action.before { @kinds = Kind.all }
  end
  
  [update, create].each do |action|
    action.wants.html { redirect_to puzzles_path }
  end
  
private
  def collection
    @collection ||= Puzzle.find :all, :order => 'kind_id, name', :include => :kind
  end
end
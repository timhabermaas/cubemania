class PuzzlesController < ResourceController::Base
  permit :admin

  belongs_to :kind
  
  [update, create].each do |action|
    action.wants.html { redirect_to kind_puzzles_path }
  end
end
class Puzzleipation < ActiveRecord::Base
  belongs_to :competition
  belongs_to :puzzle
  
  validates_uniqueness_of :competition_id, :scope => :puzzle_id
end

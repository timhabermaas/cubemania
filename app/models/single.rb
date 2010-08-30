class Single < ActiveRecord::Base
  belongs_to :user
  belongs_to :puzzle
  
  validates_presence_of :user_id, :puzzle_id, :time
end

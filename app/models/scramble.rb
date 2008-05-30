class Scramble < ActiveRecord::Base
  belongs_to :competition
  belongs_to :puzzle
  
  serialize :scrambles
  
  validates_presence_of :competiiton_id
end
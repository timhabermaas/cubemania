class Scramble < ActiveRecord::Base
  belongs_to :competition
  
  validates_presence_of :competition_id, :position, :scramble
end
class Scramble < ActiveRecord::Base
  belongs_to :competition
  
  serialize :scrambles
  
  validates_presence_of :competition_id
end
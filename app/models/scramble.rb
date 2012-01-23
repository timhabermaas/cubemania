class Scramble < ActiveRecord::Base
  validates_presence_of :position, :scramble, :competition_id
end
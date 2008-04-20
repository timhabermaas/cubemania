class Clock < ActiveRecord::Base
  belongs_to :puzzle

  validates_presence_of :time, :puzzle_id
end
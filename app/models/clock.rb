class Clock < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :user; attr_protected :user_id, :user

  validates_presence_of :time, :puzzle_id, :user_id
  validates_length_of :comment, :maximum => 255, :allow_nil => true
  validates_length_of :scramble, :maximum => 255, :allow_nil => true
end
class Clock < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :user, :counter_cache => true; attr_protected :user_id, :user

  validates_presence_of :time, :puzzle_id
end
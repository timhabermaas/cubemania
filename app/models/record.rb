class Record < ActiveRecord::Base
  belongs_to :puzzle
  belongs_to :clock, :polymorphic => true
  belongs_to :user; attr_protected :user_id, :user
  
  def created_at; clock.created_at; end
  
  validates_presence_of :time, :puzzle_id, :clock_id, :clock_type, :user_id
end
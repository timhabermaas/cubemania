class Scramble < ActiveRecord::Base
  belongs_to :matchable, :polymorphic => true
  
  validates_presence_of :position, :scramble, :matchable_id, :matchable_type
end
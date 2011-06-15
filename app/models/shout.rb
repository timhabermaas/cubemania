class Shout < ActiveRecord::Base
  belongs_to :matchable, :polymorphic => true
  belongs_to :user

  attr_accessible :content, :matchable_id, :matchable_type

  validates_presence_of :content, :user_id, :matchable_id, :matchable_type
end

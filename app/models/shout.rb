class Shout < ActiveRecord::Base
  belongs_to :matchable, :polymorphic => true
  belongs_to :user; attr_protected :user_id, :user

  validates_presence_of :content, :user_id, :matchable_id, :matchable_type
end

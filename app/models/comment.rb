class Comment < ActiveRecord::Base
  belongs_to :commentable, :polymorphic => true#, :counter_cache => true
  belongs_to :user

  attr_accessible :content, :commentable_id

  validates_presence_of :content, :commentable_id
end

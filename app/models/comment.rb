class Comment < ActiveRecord::Base
  belongs_to :commentable, :polymorphic => true
  
  validates_presence_of :content, 
end
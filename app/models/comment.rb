class Comment < ActiveRecord::Base
  belongs_to :post, :counter_cache => true
  belongs_to :user

  attr_accessible :content, :post_id

  validates_presence_of :content, :post_id
end
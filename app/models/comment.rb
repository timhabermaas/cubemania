class Comment < ActiveRecord::Base
  belongs_to :post, :counter_cache => true
  belongs_to :user; attr_protected :user_id, :user

  validates_presence_of :content, :post_id, :user_id
end
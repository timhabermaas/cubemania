class Post < ActiveRecord::Base
  has_many :comments, :order => 'created_at'
  belongs_to :user; attr_protected :user_id, :user
  
  validates_presence_of :title, :content, :user_id
end
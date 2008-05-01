class Post < ActiveRecord::Base
  belongs_to :user; attr_protected :user_id, :user
  has_many :comments, :include => :user, :order => 'comments.created_at', :dependent => :destroy

  validates_presence_of :title, :content
  validates_length_of :title, :maximum => 64
end
class Post < ActiveRecord::Base
  has_many :comments, :include => :user, :order => 'comments.created_at', :dependent => :destroy
  belongs_to :user

  attr_accessible :title, :content

  validates_presence_of :title, :content
  validates_length_of :title, :maximum => 64
end
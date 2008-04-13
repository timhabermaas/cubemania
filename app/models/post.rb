class Post < ActiveRecord::Base
  has_many :comments, :as => :commentable
  belongs_to :user; attr_protected :user_id, :user
end
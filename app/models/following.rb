class Following < ActiveRecord::Base
  belongs_to :follower, :class_name => "User", :foreign_key => :follower_id
  belongs_to :followee, :class_name => "User", :foreign_key => :followee_id

  validates_uniqueness_of :follower_id, :scope => :followee_id
end

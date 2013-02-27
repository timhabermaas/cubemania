class FollowingActivity < Activity
  belongs_to :trackable, :class_name => "Following"
end

class CreateActivity
  def self.for_following(following, activity_class=FollowActivity)
    activity_class.create :user => following.follower, :trackable => following
  end
end

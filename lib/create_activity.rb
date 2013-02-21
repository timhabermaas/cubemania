class CreateActivity
  def self.for_following(following, activity_class=FollowActivity)
    activity_class.create :user => following.follower, :trackable => following
  end

  def self.for_record(record, activity_class=RecordActivity)
    activity_class.create :user => record.user, :trackable => record
  end
end

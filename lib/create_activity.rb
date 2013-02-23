class CreateActivity
  def self.for_following(following, activity_class=FollowActivity)
    activity_class.create :user => following.follower, :trackable => following
  end

  def self.for_record(record, activity_class=RecordActivity)
    activity_class.create :user => record.user, :trackable => record
  end

  def self.for_cubing_session(session, activity_class=CubingSessionActivity)
    activity_class.create :user => session.user, :trackable => session
  end
end

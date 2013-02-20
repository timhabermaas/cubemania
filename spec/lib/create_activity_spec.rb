require_relative "../../lib/create_activity"

describe CreateActivity do
  describe ".for_following" do
    let(:user) { stub(:user) }
    let(:following) { stub(:following, :follower => user) }
    let(:activity_class) { stub(:activity_class) }

    it "creates a new FollowingActivity" do
      activity_class.should_receive(:create).with(:user => user, :trackable => following)
      CreateActivity.for_following(following, activity_class)
    end
  end
end

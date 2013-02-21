require_relative "../../lib/create_activity"

describe CreateActivity do
  let(:user) { stub(:user) }
  let(:activity_class) { stub(:activity_class) }

  describe ".for_following" do
    let(:following) { stub(:following, :follower => user) }

    it "creates a new FollowingActivity" do
      activity_class.should_receive(:create).with(:user => user, :trackable => following)
      CreateActivity.for_following(following, activity_class)
    end
  end

  describe ".for_record" do
    let(:record) { stub(:record, :user => user) }

    it "creates a new RecordActivity" do
      activity_class.should_receive(:create).with(:user => user, :trackable => record)
      CreateActivity.for_record(record, activity_class)
    end
  end
end

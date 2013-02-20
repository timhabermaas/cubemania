require "spec_helper"

describe Feed do
  let(:user) { create :user }
  let(:nick) { create :user }
  let(:peter) { create :user }
  let(:subject) { Feed.new(user) }
  let!(:activity_1) { create :follow_activity, :user => user, :created_at => DateTime.new(2013, 2, 2) }
  let!(:activity_2) { create :follow_activity, :user => nick, :created_at => DateTime.new(2013, 2, 5) }
  let!(:activity_3) { create :follow_activity, :user => peter, :created_at => DateTime.new(2013, 2, 5) }

  before do
    user.follow! nick
  end

  describe "#activities" do
    it "returns activities from the people the user follows ordered by date" do
      expect(subject.activities).to eq([activity_2, activity_1])
    end
  end
end

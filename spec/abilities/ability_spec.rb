require "spec_helper"

describe Ability do
  let(:user) { create :user }
  let(:nick) { create :user, :name => "Nick" }
  subject { Ability.new(user) }

  describe "follow" do
    it "allows following other cubers" do
      expect(subject.can? :follow, nick).to eq(true)
    end

    it "disallows following itself" do
      expect(subject.can? :follow, user).to eq(false)
    end

    it "disallows following people which are already followed" do
      user.follow!(nick)
      expect(subject.can? :follow, nick).to eq(false)
    end
  end

  describe "unfollow" do
    it "allows unfollowing people you are following" do
      user.follow!(nick)
      expect(subject.can? :unfollow, nick).to eq(true)
    end

    it "disallows unfollowing people you are not following" do
      expect(subject.can? :unfollow, nick).to eq(false)
    end

    it "disallows unfollowing yourself even if you're technically following yourself" do
      user.follow!(user)
      expect(subject.can? :unfollow, user).to eq(false)
    end
  end
end

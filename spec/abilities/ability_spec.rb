require "spec_helper"

describe Ability do
  let(:user) { create :user }
  let(:nick) { create :user, :name => "Nick" }
  subject { Ability.new(user) }

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

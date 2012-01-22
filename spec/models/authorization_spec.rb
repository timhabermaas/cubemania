require "spec_helper"

describe Authorization do
  describe "validations" do
    it { should validate_presence_of :user_id }
    it { should validate_presence_of :provider }
    it { should validate_presence_of :uid }
    it { should validate_presence_of :token }

    it "can have only one twitter account for each user" do
      user = create :user
      create :authorization, :user => user, :provider => "twitter"
      authorization = build :authorization, :user => user, :provider => "twitter", :uid => "different"
      authorization.should_not be_valid
      authorization.errors[:provider].should_not be_blank
    end
  end
end

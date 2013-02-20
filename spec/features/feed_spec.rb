require "spec_helper"

describe "Activity feed" do
  let(:user) { login }
  let(:nick) { create :user, :name => "Nick" }
  let(:peter) { create :user, :name => "Peter" }

  before do
    user.follow! nick
    user.follow! peter
    create :follow_activity, :user => peter, :trackable => create(:following, :followee => nick)
    create :follow_activity, :user => user, :trackable => create(:following, :followee => peter)
  end

  describe "follow activity" do
    it "displays a message for each following" do
      visit feed_path

      expect(page).to have_content "Peter started following Nick"
      expect(page).to have_content "#{user.name} started following Peter"
    end
  end
end

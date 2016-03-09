require "spec_helper"

describe "Comments" do
  describe "for posts" do
    before { @user = login }

    let(:post) { create :post }

    it "adds comment to post" do
      visit post_path(post)

      within "#new_comment" do
        fill_in "Content", :with => "Wow, you guys rock!"
        click_on "Respond"
      end

      within "ol.comments" do
        expect(page).to have_content "Wow, you guys rock!"
      end
    end
  end

  describe "for activity" do
    before { @user = login(as: "beta_user") }

    let(:ben) { create :user }
    let(:following) { @user.follow! ben }
    let!(:activity) { create :following_activity, :trackable => following, :user => @user }

    it "adds comment to activity" do
      visit feed_path
      within "#activity_#{activity.id}" do
        fill_in "Content", :with => "this rocks!"
        click_on "Respond"
      end

      within "#activity_#{activity.id} ol.comments" do
        expect(page).to have_content "this rocks!"
      end
    end
  end
end

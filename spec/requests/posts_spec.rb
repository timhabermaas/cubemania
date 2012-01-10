require "spec_helper"

describe "Posts" do
  before(:each) do
    login
  end

  describe "GET /posts" do
    it "display posts" do
      create(:post, :title => "Yeah")
      visit posts_path
      page.should have_content("Yeah")
    end
  end

  describe "POST /posts" do
    it "creates posts" do
      visit new_post_path
      fill_in "Title", :with => 'Hey!'
      fill_in "Content", :with => "Cubemania is awesome. True fact!"
      click_button "Create"
      page.should have_content("Cubemania is awesome")
      page.should have_content("")
    end
  end
end

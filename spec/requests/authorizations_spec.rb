require "spec_helper"

describe "Authorizations" do
  before :each do
    @user = login
  end

  describe "GET /authorizations" do
    it "displays all connected accounts" do
      create :authorization, :user => @user, :provider => "twitter"
      create :authorization, :user => @user, :provider => "facebook"
      visit authorizations_path
      page.should have_content("Twitter")
      page.should have_content("Facebook")
    end
  end

  describe "connect to Twitter" do
    it "connects user with twitter" do
      OmniAuth.config.mock_auth[:twitter] = { :provider => "twitter", :uid => "12345" }
      visit "/auth/twitter"
      page.should have_content "Successfully connected to Twitter."
      @user.authorizations.first.provider.should == "twitter"
      @user.authorizations.first.uid.should == "12345"
    end

    it "updates uid if user connects again" do
      @user.authorizations.create! :uid => "32411012", :provider => "twitter"
      OmniAuth.config.mock_auth[:twitter] = { :provider => "twitter", :uid => "1234" }
      visit "/auth/twitter"
      @user.reload.authorizations.should have(1).item
      @user.reload.authorizations.first.uid.should == "1234"
    end

    it "displays error if some information is missing" do
      OmniAuth.config.mock_auth[:twitter] = { :provider => "twitter", :uid => nil }
      visit "/auth/twitter"
      page.should have_content "Couldn\'t connect accounts."
    end

    it "displays error message if authorization doesn't work" do
      OmniAuth.config.mock_auth[:twitter] = :invalid_credentials
      visit "/auth/twitter"
      page.should have_content 'Couldn\'t connect accounts. Error: "Invalid credentials"'
    end
  end

  describe "DELETE /authorizations/:id" do#, :javascript => true do
    it "removes connection to provider" do
      authorization = create :authorization, :provider => "Twitter", :user => @user
      visit authorizations_path
      click_on "Disconnect"
      page.should have_content "Successfully disconnected from Twitter."
      @user.reload.authorizations.should == []
    end
  end
end

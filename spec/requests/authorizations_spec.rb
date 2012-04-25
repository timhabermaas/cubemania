require "spec_helper"

describe "Authorizations" do
  before :each do
    @user = login
  end

  describe "GET /authorizations" do
    it "displays all connected accounts" do
      create :authorization, :user => @user, :provider => "facebook"
      visit authorizations_path
      page.should have_content("Facebook")
    end
  end

  describe "POST /authorizations" do
    before :each do
      OmniAuth.config.mock_auth[:facebook] = { :provider => "facebook",
                                               :uid => "54321",
                                               :credentials => { :token => "agsasfa", :secret => "addsafasers41s" } }
    end
    describe "Facebook" do
      it "connects user with Facebook" do
        visit "/auth/facebook"
        page.should have_content "Successfully connected to Facebook."
        @user.authorizations.first.provider.should == "facebook"
        @user.authorizations.first.uid.should == "54321"
      end

      it "updates uid if user connects again" do
        create :authorization, :user => @user, :uid => "32411012", :provider => "facebook"
        visit "/auth/facebook"
        @user.reload.authorizations.should have(1).item
        @user.reload.authorizations.first.uid.should == "54321"
      end

      it "displays error if some information is missing" do
        OmniAuth.config.mock_auth[:facebook] = { :provider => "facebook", :uid => nil }
        visit "/auth/facebook"
        page.should have_content "Couldn\'t connect accounts."
      end

      it "displays error message if authorization doesn't work" do
        OmniAuth.config.mock_auth[:facebook] = :invalid_credentials
        visit "/auth/facebook"
        page.should have_content 'Couldn\'t connect accounts. Error: "Invalid credentials"'
      end
    end
  end

  describe "DELETE /authorizations/:id" do#, :javascript => true do
    it "removes connection to provider" do
      authorization = create :authorization, :provider => "Facebook", :user => @user
      visit authorizations_path
      click_on "Disconnect"
      page.should have_content "Successfully disconnected from Facebook."
      @user.reload.authorizations.should == []
    end
  end
end

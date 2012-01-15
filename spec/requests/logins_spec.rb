require "spec_helper"

describe "Logins" do
  describe "POST /logins" do
    before :each do
      create :user, :name => "peter", :password => "secret", :password_confirmation => "secret"
    end

    it "logs user in successfully" do
      visit login_path
      fill_in "Name", :with => "peter"
      fill_in "Password", :with => "secret"
      click_button "Login"
      page.should have_content "Hello peter, you are now logged in"
    end
  end
end

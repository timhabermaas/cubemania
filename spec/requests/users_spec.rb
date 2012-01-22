require 'spec_helper'

describe "Users" do
  describe "POST /users" do
    it "creates a new user" do
      visit new_user_path
      fill_in "Name", :with => "rowe"
      fill_in "Email", :with => "rowe@awesome.com"
      fill_in "Password", :with => "password"
      fill_in "Confirmation", :with => "password"

      click_button "Register"

      page.should have_content "rowe's Profile"
      page.should have_content "Hello rowe, you are now registered."
      ActionMailer::Base.deliveries.last.to.should include("rowe@awesome.com")
    end
  end

  describe "GET /users" do
    it "lists users" do
      create :user, :name => "ryan"
      create :user, :name => "rowe"
      visit users_path

      page.should have_content "ryan"
      page.should have_content "rowe"
    end
  end

  describe "PUT /users" do
    it "updates users's name" do
      user = login
      visit user_path(user)
      fill_in "Name", :with => "Peter"
      click_button "Update"

      within("#user") do
        page.should have_content "Peter"
      end
    end
  end
end

require 'spec_helper'

describe "PasswordResets" do
  it "sends email after entering existing email" do
    user = create :user
    visit login_path
    click_on "Forgot your password?"
    fill_in "Email", :with => user.email
    click_button "Reset"
    page.should have_content "Email sent successfully"
    ActionMailer::Base.deliveries.last.to.should include(user.email)
  end

  it "doesn't send email if email does not exist" do
    visit login_path
    click_on "Forgot your password?"
    fill_in "Email", :with => "doesntexist@foo.com"
    click_button "Reset"
    current_path.should == reset_password_path
    page.should have_content("Email does not exist.")
  end

  it "finds email even if it doesn't match casewise" do
    user = create :user, :email => "muh@Cow.com"
    visit new_reset_password_path
    fill_in "Email", :with => "muh@cow.com"
    click_button "Reset"
    page.should have_content "Email sent successfully"
  end
end

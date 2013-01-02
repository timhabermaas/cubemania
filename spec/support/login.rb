module Capybara
  module SessionHelper
    def login(options = { :as => "user" })
      user = create :user, :password => "secret", :password_confirmation => "secret", :role => options[:as]
      visit login_path
      fill_in "Name", :with => user.name
      fill_in "Password", :with => "secret"
      click_button "Login"
      user
    end

    def login_via_rack_test
      user = create :user, :password => "muhmuh", :password_confirmation => "muhmuh"
      page.driver.post "/session", { :login => { :name => user.name, :password => "muhmuh" } }
      user
    end
  end
end

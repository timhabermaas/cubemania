require "spec_helper"

describe "Users" do
  describe "GET /users" do
    let!(:user_1) { create :user, :name => "rowe", :singles_count => 100 }
    let!(:user_2) { create :user, :name => "sarah", :singles_count => 5000 }

    it "displays users in a list ordered by singles" do
      visit users_path

      within("li", :text => "rowe") do
        expect(page).to have_content "100"
      end
      within("li", :text => "sarah") do
        expect(page).to have_content "5000"
      end

      expect(page.body).to match(/sarah.*rowe/m)
    end

    it "lets you filter by name" do
      visit users_path(:q => "ro")

      expect(page).to have_content "rowe"
      expect(page).to_not have_content "sarah"
    end

    describe "pagination" do
      let!(:lonely_user) { create :user, :name => "lonely_dude", :singles_count => 0 }

      before :each do
        create_list :user, 200, :singles_count => 2
      end

      it "redirects to a new page with more names on it" do
        visit users_path

        expect(page).to_not have_content "lonely_dude"
        click_on "Show more"
        expect(page).to have_content "lonely_dude"
      end
    end
  end

  describe "GET /users/:id" do
    let(:puzzle)  { create :puzzle, :name => "3x3x3" }
    let(:puzzle2) { create :puzzle, :name => "4x4x4" }
    let(:user)    { create :user, :name => "rowe", :wasted_time => 14153 }

    it "displays the name and wasted time" do
      visit user_path(user)
      within("#content h1") do
        expect(page).to have_content "rowe"
        expect(page).to have_content "has spent about 4 hours solving puzzles"
      end
    end

    describe "followings" do
      let(:nick) { create :user, :name => "Nick" }

      before do
        user.follow!(nick)
      end

      it "lists the names of all cubers he follows" do
        visit user_path(user)
        within("ul.followees") do
          expect(page).to have_content "Nick"
        end
      end

      it "lists the names of all cubers who follow him" do
        visit user_path(nick)
        within("ul.followers") do
          expect(page).to have_content "rowe"
        end
      end
    end

    context "having records" do
      let!(:record)  { create :record, :user => user, :puzzle => puzzle, :amount => 5, :time => 14500 }
      let!(:record2) { create :record, :user => user, :puzzle => puzzle2, :amount => 12, :time => 22300 }

      it "displays his 3x3x3 record" do
        visit user_path(user)

        within("ul.records") do
          within("li", :text => "3x3x3") do
            expect(page).to have_content "14.50s"
          end
          within("li", :text => "4x4x4") do
            expect(page).to have_content "22.30s"
          end
        end
      end
    end
  end


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

  describe "PUT /users" do
    it "updates users's name" do
      user = login
      visit user_path(user)
      click_on "Edit Profile"

      fill_in "Name", :with => "Peter"
      click_button "Update"

      expect(user.reload.name).to eq("Peter")
    end
  end
end

require "spec_helper"

describe "Times" do
  let(:puzzle) { create :puzzle }

  before :each do
    @user = login :as => "user"
  end

  describe "GET /" do
    before :each do
      create(:single, :puzzle => puzzle, :user => @user, :time => 12340)
      create(:single, :puzzle => puzzle, :user => @user, :time => 41230)
    end

    it "displays at least the time" do
      visit puzzle_times_path(puzzle)
      within("#singles") do
        page.should have_content "12.34"
        page.should have_content "41.23"
      end
    end
  end

  describe "POST /" do
    it "posts single and shows it on page" do
      visit puzzle_times_path(puzzle)
      fill_in "Time", :with => "12.32"
      click_on "Submit"
      page.should have_content "12.32"
    end
  end
end

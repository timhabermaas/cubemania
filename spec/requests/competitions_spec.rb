=begin
require "spec_helper"

describe "Competitions" do
  before :each do
    @user = login
  end
  let(:puzzle) { create :puzzle, :attempt_count => 5 }

  describe "POST /competitions" do
    it "creates competition successfully" do
      visit puzzle_competitions_path(puzzle)
      fill_in "Name", :with => "Jam 2012"
      fill_in "Description", :with => "Rock it!"
      select "Daily", :from => "Repeat"
      select "Beginner", :from => "Skill"
      click_button "Create"

      page.should have_content "Successfully created competition."
      page.should have_content "Jam 2012"
    end
  end

  describe "Compete" do
    let(:competition) { create :competition, :puzzle => puzzle }

    it "enter all solves, click on submit and see your average" do
      visit puzzle_competition_path(puzzle, competition)
      fill_in "Solve 1", :with => "12.32"
      fill_in "Solve 2", :with => "14.32"
      fill_in "Solve 3", :with => "16.42"
      fill_in "Solve 4", :with => "11.94"
      fill_in "Solve 5", :with => "13.44"
      click_button "Submit Average"
      within "#results" do
        page.should have_content @user.name
        page.should have_content "13.36"
      end
    end
  end
end
=end

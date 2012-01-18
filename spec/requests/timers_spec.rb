require "spec_helper"

describe "Timer" do
  before :each do
    @user = login :as => "user"
  end

  let(:puzzle) { create :puzzle, :name => "3x3x3" }

  describe "adding times" do
    before :each do
      create :single, :user => @user, :puzzle => puzzle
      create :single, :user => @user, :puzzle => puzzle
    end

    it "adds time to top of singles after submitting time" do
      visit puzzle_timers_path(puzzle)
      fill_in "Time", :with => "1:12.32"
      click_button "Submit"
      within "#singles" do
        find(:li, 1).text.should match("1:12.32")
      end
    end

    it "updates scramble after submitting time" do
      visit puzzle_timers_path(puzzle)
      old_scramble = find("#timer .scramble").text
      fill_in "Time", :with => "13.37"
      click_button "Submit"
      find("#timer .scramble").text.should_not == old_scramble
    end
  end

  describe "best average of 5" do
    before :each do
      create :single, :user => @user, :puzzle => puzzle, :time => 11000
      3.times { create :single, :user => @user, :puzzle => puzzle, :time => 10000 }
      create :single, :user => @user, :puzzle => puzzle, :time => 5000
      # (11.00) 10.00 10.00 10.00 (5.00)
    end

    it "displays best average of 5" do
      visit puzzle_timers_path(puzzle)
      within "#stats .best" do
        page.should have_content "10.00"
      end
    end

    it "updates the record after submitting a faster time" do
      visit puzzle_timers_path(puzzle)
      fill_in "Time", :with => "5.00"
      click_button "Submit"
      # 10.00 10.00 10.00 (5.00) 5.00
      within "#stats .best" do
        page.should have_content "8.33"
      end
    end
  end
end

require "spec_helper"

describe "records" do
  let(:puzzle) { create :puzzle }
  let!(:record) { create :record, :puzzle => puzzle, :amount => 5, :time => 12510 }
  let!(:record2) { create :record, :puzzle => puzzle, :amount => 5, :time => 15230 }
  let!(:record3) { create :record, :puzzle => puzzle, :amount => 1, :time => 13370 }

  describe "GET /users/jon/records/141" do
    it "lists all singles and average time for that record" do
      visit user_record_path(record.user, record)
      expect(page).to have_content "12.51"
      5.times do |i|
        expect(page).to have_content record.singles[i].human_time
      end
      expect(page).to have_content record.human_time
    end
  end

  describe "GET /puzzles/3x3x3/records/" do
    it "lists all records of all people for the 3x3x3" do
      visit puzzle_records_path(puzzle, :type => "avg5")

      expect(page).to have_content "12.51"
      expect(page).to have_content "15.23"
      expect(page).to_not have_content "13.37"
    end
  end
end

require "spec_helper"

describe "Records" do
  let(:puzzle) { create :puzzle }
  let(:record) { create :record, :puzzle => puzzle, :amount => 5 }

  it "lists all singles and average time for that record" do
    visit puzzle_record_path(puzzle, record)
    5.times do |i|
      page.should have_content record.singles[i].human_time
    end
    page.should have_content record.human_time
  end
end

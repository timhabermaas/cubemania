require "spec_helper"

describe "api/puzzles/:id/singles" do
  describe "chart" do
    let(:puzzle) { create(:puzzle) }
    let(:user2) { create(:user) }

    let(:first_of_december) { DateTime.new(2012, 12, 1) }
    let(:eleventh_of_december) { DateTime.new(2012, 12, 11) }
    let(:first_of_april) { DateTime.new(2012, 4, 1) }
    let(:third_of_april) { DateTime.new(2012, 4, 3) }

    before(:each) do
      @user = login_via_rack_test
      create :single, :user => @user, :puzzle => puzzle, :created_at => first_of_december, :time => 2000
      create :single, :user => @user, :puzzle => puzzle, :created_at => eleventh_of_december, :time => 3000
      create :single, :user => @user, :puzzle => puzzle, :created_at => first_of_april, :time => 10000
      create :single, :user => @user, :puzzle => puzzle, :created_at => third_of_april, :time => 20000
      create :single, :user => user2, :puzzle => puzzle, :created_at => third_of_april, :time => 20000
    end

    describe "no date range given" do
      it "returns singles grouped by month" do
        page.driver.get "/api/puzzles/#{puzzle.id}/singles/chart.json", :user_id => @user.id
        result = JSON.parse(page.driver.response.body)
        expect(result).to have(2).items
        expect(result[0]["time"]).to eq(2500)
        expect(result[1]["time"]).to eq(15000)
      end
    end
  end
end

require "spec_helper"

describe "api/puzzles/:id/singles", :type => :api do
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
        get "/legacy_api/puzzles/#{puzzle.id}/singles/chart.json", :user_id => @user.id
        result = JSON.parse(last_response.body)
        expect(result).to have(2).items
        expect(result[0]["time"]).to eq(15000)
        expect(result[1]["time"]).to eq(2500)
      end
    end

    describe "date range of one month" do
      it "returns singles grouped by day" do
        get "/legacy_api/puzzles/#{puzzle.id}/singles/chart.json", :user_id => @user.id,
                                                            :from => Time.new(2012, 4, 1).to_i,
                                                            :to => Time.new(2012, 5, 1).to_i
        result = JSON.parse(last_response.body)
        expect(result).to have(2).items
        expect(result[0]["time"]).to eq(10000)
        expect(result[1]["time"]).to eq(20000)
      end
    end

    describe "time zone awareness" do
      let(:time_zone) { ActiveSupport::TimeZone.new("Berlin") }
      let(:user) { create :user, :time_zone => "Berlin" }

      it "groups by time zone of user" do
        create :single, :user => user, :puzzle => puzzle, :time => 13, :created_at => time_zone.local(2012, 10, 4, 23, 30)
        create :single, :user => user, :puzzle => puzzle, :time => 15, :created_at => time_zone.local(2012, 10, 5, 0, 30)

        get "/legacy_api/puzzles/#{puzzle.id}/singles/chart.json", :user_id => user.id,
                                                            :from => Time.new(2012, 10, 1).to_i,
                                                            :to => Time.new(2012, 10, 30).to_i
        result = JSON.parse(last_response.body)
        expect(result).to have(2).items
        expect(result[0]["time"]).to eq(13)
        expect(result[1]["time"]).to eq(15)
      end
    end
  end
end

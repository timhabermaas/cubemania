require "spec_helper"

describe "/api/puzzles/:id/singles", :type => :api do
  let!(:puzzle) { create(:puzzle, :name => "3x3x3") }

  describe "POST /" do
    before do
      @user = login_via_rack_test
      post "/api/puzzles/#{puzzle.id}/singles.json", :single => parameters
    end

    context "valid single" do
      let(:parameters) { {:time => 14500, :scramble => "R2 D2"} }

      it "returns 201 Created and the created single" do
        expect(last_response.status).to eq(201)
        expect(json_response["time"]).to eq(14500)
        expect(json_response["scramble"]).to eq("R2 D2")
      end

      it "creates a new record" do
        r = Record.all.first
        expect(r.time).to eq(14500)
        expect(r.amount).to eq(1)
        expect(r.puzzle_id).to eq(puzzle.id)
      end

      it "responds with X-New-Record='true'" do
        expect(last_response.headers["X-New-Record"]).to eq("true")
      end
    end

    context "invalid single" do
      let(:parameters) { {:scramble => "R2 D2"} }

      it "returns 422" do
        expect(last_response.status).to eq(422)
        expect(json_response["errors"]["time"][0]).to eq("can't be blank")
      end
    end
  end

  # TODO move most of this stuff into unit tests
  describe "GET chart" do
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
        get "/api/puzzles/#{puzzle.id}/singles/chart.json", :user_id => @user.id
        expect(json_response).to have(2).items
        expect(json_response[0]["time"]).to eq(15000)
        expect(json_response[1]["time"]).to eq(2500)
      end
    end

    describe "date range of one month" do
      it "returns singles grouped by day" do
        get "/api/puzzles/#{puzzle.id}/singles/chart.json", :user_id => @user.id,
                                                            :from => Time.new(2012, 4, 1).to_i,
                                                            :to => Time.new(2012, 5, 1).to_i
        expect(json_response).to have(2).items
        expect(json_response[0]["time"]).to eq(10000)
        expect(json_response[1]["time"]).to eq(20000)
      end
    end

    describe "time zone awareness" do
      let(:time_zone) { ActiveSupport::TimeZone.new("Berlin") }
      let(:user) { create :user, :time_zone => "Berlin" }

      it "groups by time zone of user" do
        create :single, :user => user, :puzzle => puzzle, :time => 13, :created_at => time_zone.local(2012, 10, 4, 23, 30)
        create :single, :user => user, :puzzle => puzzle, :time => 15, :created_at => time_zone.local(2012, 10, 5, 0, 30)

        get "/api/puzzles/#{puzzle.id}/singles/chart.json", :user_id => user.id,
                                                            :from => Time.new(2012, 10, 1).to_i,
                                                            :to => Time.new(2012, 10, 30).to_i
        expect(json_response).to have(2).items
        expect(json_response[0]["time"]).to eq(13)
        expect(json_response[1]["time"]).to eq(15)
      end
    end
  end
end

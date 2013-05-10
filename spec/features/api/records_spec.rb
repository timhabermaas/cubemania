require "spec_helper"

describe "/api/puzzles/3x3x3/records" do
  let(:puzzle) { create :puzzle }
  let(:user) { create :user }
  let(:user_2) { create :user }

  describe "GET /" do
    context "user_id given" do
      before do
        create :record, :user => user,
                        :puzzle => puzzle,
                        :amount => 5,
                        :time => 51400
        create :record, :user => user,
                        :puzzle => puzzle,
                        :amount => 12,
                        :time => 65000
        create :record, :user => user_2,
                        :puzzle => puzzle,
                        :amount => 5,
                        :time => 1337

        get "/api/puzzles/#{puzzle.id}/records.json", :user_id => user.id
      end

      it "returns all records for this user" do
        expect(last_response.status).to eq(200)
        expect(json_response).to have(2).items
        expect(json_response[0]["time"]).to eq(51400)
        expect(json_response[1]["time"]).to eq(65000)
      end
    end
  end

  describe "GET /recent" do
    before do
      create :record, :user => user,
                      :puzzle => puzzle,
                      :amount => 5,
                      :time => 51400,
                      :set_at => DateTime.new(2013, 5, 1)
      create :record, :user => user,
                      :puzzle => puzzle,
                      :amount => 12,
                      :time => 65000,
                      :set_at => DateTime.new(2013, 5, 2)
      create :record, :user => user,
                      :puzzle => puzzle,
                      :amount => 5,
                      :time => 1337,
                      :set_at => DateTime.new(2013, 5, 3)

      get "/api/puzzles/#{puzzle.id}/records/recent.json", :user_id => user.id, :format => :json
    end

    it "returns only the recent records for each record type" do
      expect(last_response.status).to eq(200)
      expect(json_response).to have(2).items
      expect(json_response[0]["time"]).to eq(1337)
      expect(json_response[0]["amount"]).to eq(5)
      expect(json_response[1]["time"]).to eq(65000)
      expect(json_response[1]["amount"]).to eq(12)
    end
  end
end

require "spec_helper"

describe "/api/puzzles/3x3x3/records" do
  let(:puzzle) { create :puzzle }
  let(:user) { create :user }
  let(:user_2) { create :user }

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
      expect(json_response).to have(2).items
      expect(json_response[0]["time"]).to eq(51400)
      expect(json_response[1]["time"]).to eq(65000)
    end
  end
end

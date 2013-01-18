require "spec_helper"

describe "Access", :type => :api do
  it "renders a 401 response when user's not logged in" do
    post "/api/puzzles/3x3x3/singles.json"
    expect(last_response.status).to eq(401)
  end
end

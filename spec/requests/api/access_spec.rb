require "spec_helper"

describe "Access" do
  it "renders a 401 response when user's not logged in" do
    post "/api/puzzles/3x3x3/singles.json", {:single => {:time => 1000}}
    expect(response.status).to eq(401)
  end
end

require "spec_helper"

describe "Access" do
  it "renders a 401 response when user's not logged in" do
    page.driver.post "/api/puzzles/3x3x3/singles.json"
    expect(page.driver.status_code).to eq(401)
  end
end

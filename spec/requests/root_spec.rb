require "spec_helper"

describe "Root" do
  it "shows the latest news" do
    create :post, :content => "We launched Cubemania! FUCK YEAH!"
    create :post, :content => "Be careful! Cubing is addictive!"
    visit root_path
    page.should have_content("Be careful!")
  end
end

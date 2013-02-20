require "spec_helper"

describe "Following" do
  before do
    @user = login
    @peter = create :user, :name => "Peter"
  end

  it "allows users to follow each other" do
    visit user_path(@peter)

    click_on "Follow Peter"
    expect(page).to have_content "You are now following Peter"
    expect(@peter.followers).to eq([@user])
  end

  it "allows users to unfollow them again" do
    visit user_path(@peter)

    click_on "Follow Peter"
    click_on "Unfollow Peter"
    expect(page).to have_content "You are no longer following Peter"
    expect(@peter.followers).to eq([])
  end
end

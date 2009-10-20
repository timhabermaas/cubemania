require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

describe Match, "validation and security" do
  before(:each) do
  end

  it "should be valid given valid attributes" do
    match = Factory.build(:match)
    match.should be_valid
  end
  
  it "should not be valid missing a user" do
    match = Factory.build(:match, :user => nil)
    match.should_not be_valid
    match.errors.on(:user_id).should =~ /blank/
  end
  
  it "should not be valid missing an opponent" do
    match = Factory.build(:match, :opponent => nil)
    match.should_not be_valid
    match.errors.on(:opponent_id).should =~ /blank/
  end
  
  it "should not be valid missing a puzzle" do
    match = Factory.build(:match, :puzzle => nil)
    match.should_not be_valid
    match.errors.on(:puzzle_id).should =~ /blank/
  end
  
  it "should not change the user through mass assignment" do
    match = Match.new({:user_id => 2})
    match.user_id.should be_nil
    match = Match.new({:user => Factory.build(:user)})
    match.user.should be_nil
  end
  
  it "should not allow users to challenge themselves" do
    user = Factory.create(:user)
    match = Factory.build(:match, :user => user, :opponent => user)
    match.should_not be_valid
  end
end

describe Match, "scrambles" do
  it "should save 5 scrambles for a 3x3x3 match" do
    match = Factory.create(:match, :puzzle => Factory.create(:puzzle, :attempt_count => 5))
    match = Match.find match.id
    match.scrambles.should have(5).scrambles
  end
end

describe Match, "named_scopes" do
  it "should return all matches for user" do
    user = Factory.create(:user, :name => 'tim')
    match1 = Factory.create(:match, :user => user)
    match2 = Factory.create(:match, :opponent => user)
    match3 = Factory.create(:match)
    Match.for(user).should have(2).matches
    Match.for(user).should include(match1)
    Match.for(user).should include(match2)
    Match.for(user).should_not include(match3)
  end
  
  it "should return all finished matches" do
    user1 = Factory.create(:user)
    user2 = Factory.create(:user)
    unfinished_match = Factory.create(:match)
    finished_match = Factory.create(:match, :user => user1, :opponent => user2)
    Factory.create(:average, :match => finished_match, :user => user1)
    Factory.create(:average, :match => finished_match, :user => user2)
    Match.finished.should include finished_match
    Match.finished.should_not include unfinished_match
  end
  
  it "should return all challenged matches" do
    user1 = Factory.create(:user)
    user2 = Factory.create(:user)
    unfinished_match = Factory.create(:match)
    challenged_match = Factory.create(:match, :user => user1)
    finished_match = Factory.create(:match, :user => user1, :opponent => user2)
    Factory.create(:average, :match => finished_match, :user => user1)
    Factory.create(:average, :match => finished_match, :user => user2)
    Factory.create(:average, :match => challenged_match, :user => user1)
    Match.challenged.should include challenged_match
    Match.challenged.should_not include finished_match
    Match.challenged.should_not include unfinished_match
  end
end

describe Match do
  it "should return the proper opponent name" do
    user = Factory.create(:user, :name => 'tim')
    opponent = Factory.create(:user, :name => 'simon')
    match = Factory.build(:match, :user => user, :opponent => opponent)
    match.opponent_name_for(user).should == 'simon'
    match.opponent_name_for(opponent).should == 'tim'
  end
end

describe Match, "status" do
  before(:each) do
    @user = Factory.create(:user)
    @opponent = Factory.create(:user)
    Clock.destroy_all
  end
  
  it "should have status 'pending' after creation" do
    match = Factory.create(:match)
    match.should be_pending
    match.should_not be_finished
    match.should_not be_challenged
  end
  
  it "should have status 'challenged' after user has submitted his time" do
    match = Factory.create(:match, :user => @user)
    Factory.create(:average, :user => @user, :match => match)
    match.should be_challenged
    match.should_not be_finished
    match.should_not be_pending
  end
  
  it "should have status 'finished' after both users has submitted their times" do
    match = Factory.create(:match, :user => @user, :opponent => @opponent)
    Factory.create(:average, :user => @user, :match => match)
    Factory.create(:average, :user => @opponent, :match => match)
    match.should be_finished
    match.should_not be_pending
    match.should_not be_challenged
  end
end
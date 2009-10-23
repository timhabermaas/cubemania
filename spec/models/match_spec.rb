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
  
  it "should not allow other statuses than 'pending', 'challenged' and 'finished'" do
    match = Factory.build(:match, :status => 'other')
    match.should_not be_valid
  end
end

describe Match, "scrambles" do
  it "should save 5 scrambles for a 3x3x3 match" do
    match = Factory.create(:match, :puzzle => Factory.create(:puzzle, :attempt_count => 5))
    match = Match.find match.id
    match.scrambles.should have(5).scrambles
  end
  
  it "should deliver the same scrambles for every call" do
    match = Factory.create(:match, :puzzle => Factory.create(:puzzle, :attempt_count => 5))    
    first_scrambles = match.scrambles.collect(&:scramble)
    second_scrambles = match.scrambles.collect(&:scramble)
    first_scrambles.should == second_scrambles
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
    user_1 = Factory.create(:user)
    user_2 = Factory.create(:user)
    unfinished_match = Factory.create(:match)
    challenged_match = Factory.create(:match, :user => user_1)
    finished_match = Factory.create(:match, :user => user_1, :opponent => user_2)
    Factory.create(:average, :match => finished_match, :user => user_1)
    Factory.create(:average, :match => finished_match, :user => user_2)
    Factory.create(:average, :match => challenged_match, :user => user_1)
    Match.challenged.should include challenged_match
    Match.challenged.should_not include finished_match
    Match.challenged.should_not include unfinished_match
  end
end

describe Match, "public methods" do
  it "should return the proper opponent name" do
    user = Factory.create(:user, :name => 'tim')
    opponent = Factory.create(:user, :name => 'simon')
    match = Factory.build(:match, :user => user, :opponent => opponent)
    match.opponent_name_for(user).should == 'simon'
    match.opponent_name_for(opponent).should == 'tim'
  end
end

describe Match, "winner and loser" do
  before(:each) do
    @user_1 = Factory.create(:user)
    @user_2 = Factory.create(:user)
    @match = Factory.create(:match, :user => @user_1, :opponent => @user_2)
  end
  
  it "should provide a winner and a loser if match is finished" do
    Factory.create(:average, :user => @user_1, :time => 1230, :match => @match)
    Factory.create(:average, :user => @user_2, :time => 2320, :match => @match)
    @match.winner.should == @user_1
    @match.loser.should == @user_2
  end
  
  it "should return no winner nor loser if the times are equal" do
    Factory.create(:average, :user => @user_1, :time => 1230, :match => @match)
    Factory.create(:average, :user => @user_2, :time => 1230, :match => @match)
    @match.winner.should be_nil
    @match.loser.should be_nil
  end
  
  it "should not crash if one user hasn't submitted his time yet" do
    Factory.create(:average, :user => @user_1, :time => 20, :match => @match)
    lambda { @match.winner }.should_not raise_error
    lambda { @match.loser }.should_not raise_error
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
    lambda { Factory.create(:average, :user => @user, :match => match) }.should change(match, :status).
      from('pending').to('challenged')
  end
  
  it "should have status 'finished' after both users has submitted their times" do
    match = Factory.create(:match, :user => @user, :opponent => @opponent)    
    Factory.create(:average, :user => @user, :match => match)
    lambda { Factory.create(:average, :user => @opponent, :match => match) }.should change(match, :status).
      from('challenged').to('finished') 
  end
end

describe Match, "ELO rating system" do
  it "should share 30 points given two equally strong players" do
    match = Factory.create(:match)
    match.max_win(match.user).should == 15
    match.max_loss(match.user).should == -15
    match.max_win(match.opponent).should == 15
    match.max_loss(match.opponent).should == -15
  end
  
  it "should gain 27 points winning a match between 1400 and 1000 points" do
    match = Factory.create(:match, :user => Factory.create(:user, :points => 1400),
            :opponent => Factory.create(:user, :points => 1000))
    match.max_win(match.user).should == 3
    match.max_loss(match.user).should == -27
    match.max_win(match.opponent).should == 27
    match.max_loss(match.opponent).should == -3
  end
  
  it "should give the user 15 points if he owns completely (other user gets a dnf)" do
    match = Factory.create(:match)
    lambda do
      Factory.create(:average, :match => match, :user => match.user)
      Factory.create(:average, :match => match, :user => match.opponent, :dnf => true)
    end.should change(match.user, :points).by(15)
  end
  
  it "should not change points for a deuce and equally strong users" do
    match = Factory.create(:match)
    lambda do
      Factory.create(:average, :match => match, :user => match.user, :time => 2000)
      Factory.create(:average, :match => match, :user => match.opponent, :time => 2000)
    end.should_not change(match.user, :points)
    Average.destroy_all
    lambda do
      Factory.create(:average, :match => match, :user => match.user, :time => 2000)
      Factory.create(:average, :match => match, :user => match.opponent, :time => 2000)
    end.should_not change(match.opponent, :points)
  end
  
  it "should not change points once they've been evaluated"
end
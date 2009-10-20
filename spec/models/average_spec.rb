require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

describe Average do
  
  it "should create a new instance given valid attributes" do
    average = Factory.build(:average)
    average.should be_valid
  end
  
  it "should give an error if there are not enough singles for an average" do
    average = Factory.build(:average)
    average.singles.delete(average.singles.first)
    average.should_not be_valid
  end
  
end

describe Average, "comparison" do
  
  it "should be properly comparable" do
    Factory.build(:average, :dnf => true).should be == Factory.build(:average, :dnf => true)
    Factory.build(:average, :dnf => false).should be < Factory.build(:average, :dnf => true)
    Factory.build(:average, :dnf => true).should be > Factory.build(:average, :dnf => false)
    Factory.build(:average, :time => 10).should be < Factory.build(:average, :time => 20)
    Factory.build(:average, :time => 30).should be > Factory.build(:average, :time => 20)
    Factory.build(:average, :time => 20).should be == Factory.build(:average, :time => 20)
  end
  
end

describe Average, "match validations" do
  
  before(:each) do
    @user = Factory.create(:user)
    @opponent = Factory.create(:user)
  end
  
  it "should create a new instance given valid attributes" do
    average = Factory.build(:average_with_match)
    average.should be_valid
  end
  
  it "should not allow submissions of users who already submitted their time" do
    match = Factory.create(:match, :opponent => @user)
    Factory.create(:average, :user => @user, :match => match)
    average = Factory.build(:average, :user => @user, :match => match)
    average.should_not be_valid
  end
  
  it "should not allow submissions of opponents who already submitted their time" do
    match = Factory.create(:match, :opponent => @user)
    Factory.create(:average, :user => @user, :match => match)
    average = Factory.build(:average, :user => @user, :match => match)
    average.should_not be_valid
  end
  
  it "should not allow submissions of other users than the users registered for the match" do
    third_person = Factory.create(:user)
    match = Factory.create(:match, :user => @user, :opponent => @opponent)
    average = Factory.build(:average, :user => third_person, :match => match)
    average.should_not be_valid
  end
end
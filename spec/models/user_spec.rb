require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

describe User do

  it "should create a new instance given valid attributes" do
    user = Factory.build(:user)
    user.should be_valid
  end
  
  it "should give an error given a wrong password_confirmation" do
    user = Factory.build(:user, :password_confirmation => 'blub')
    user.should_not be_valid
    user.should have(1).error_on(:password)
  end
  
  it "should require a minimum length of 5 for a password" do
    user = Factory.build(:user, :password => 'sho')
    user.should_not be_valid
    user.should have(1).error_on(:password)
  end
end

describe User, "json and xml" do
  
  it "should not display sensible information via json" do
    user = Factory.create(:user)
    user.to_json.should_not =~ /password/
    user.to_json.should_not =~ /salt/
    user.to_json.should_not =~ /ignored/
    user.to_json.should_not =~ /email/
    user.to_json.should_not =~ /role/
  end
  
end
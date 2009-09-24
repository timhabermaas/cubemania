require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

describe User do

  it "should create a new instance given valid attributes" do
    user = Factory.build(:user)
    user.should be_valid
  end
  
  it "should give an error given a wrong password_confirmation" do
    user = Factory.build(:user, :password_confirmation => 'blub')
    user.should_not be_valid
    user.errors.on(:password).should eql('should match confirmation')
  end
end

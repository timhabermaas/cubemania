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
  
  it "should require a minimum length of 4 for a password" do
    user = Factory.build(:user, :password => 'sho')
    user.should_not be_valid
    user.should have(1).error_on(:password)
  end
  
  it "should not allow two users with the same name" do
    user1 = Factory.create(:user, :name => 'peter')
    user2 = Factory.build(:user, :name => 'peter')
    user1.should be_valid
    user2.should_not be_valid
  end
  
  it "should not allow two useres with the same email" do
    user1 = Factory.create(:user, :email => 'peter@test.com')
    user2 = Factory.build(:user, :email => 'peter@test.com')
    user1.should be_valid
    user2.should_not be_valid
  end
  
  it "should not allow invalid email addresses" do
    invalid_emails = ['foo@bar.', 'foo@bar.de.', 'foo.de.de', '@bar.de']
    invalid_emails.each do |email|
      user = Factory.build(:user, :email => email)
      user.should_not be_valid
    end
  end
  
end

describe User, "to_json" do
  
  before(:each) do
    @user = Factory.create(:user, :name => 'peter', :email => 'peter@doc.com', :wca => '2007JDAE01')
    @user_hash = ActiveSupport::JSON.decode(@user.to_json)
    @forbidden_attributes = [:password, :ignored, :role, :salt, :email, :created_at, :sponsor]
    @necessary_attributes = [:id, :averages_count, :time_zone, :wca, :name]
  end
  
  it "should not display sensible information via json" do
    @forbidden_attributes.each do |attribute|
      @user_hash.keys.should_not include(attribute.to_s)
    end
  end
  
  it "should contain necessary informations about a user" do
    @necessary_attributes.each do |attribute|
      @user_hash.keys.should include(attribute.to_s)
    end
  end
  
  it "should contain proper values" do
    @user_hash['name'].should == 'peter'
    @user_hash['averages_count'].should == 0
    @user_hash['wca'].should == '2007JDAE01'
  end
  
end

describe User, "password" do
  
  before(:each) do
    @user = Factory.create(:user)
  end
  
  it "should flash the password after save" do
    @user.password.should be_nil
    @user.password_confirmation.should be_nil
  end
  
  it "should reset the password to a at least 12 characters long string"do
    @user.reset_password!
    @user.password.should =~ /[a-z0-9A-Z]{12}/
  end
  
end

describe User, "matches association" do
  
  it "should find all matches for this user" do
    user = Factory.create(:user)
    match_1 = Factory.create(:match, :user => user)
    match_2 = Factory.create(:match, :opponent => user)
    user.matches.size.should == 2
    user.matches.include?(match_1).should be_true
    user.matches.include?(match_2).should be_true
  end
  
end

describe User, "ranking" do
end
require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

describe User do

  describe "validations" do
    def user
      @user ||= Factory.build(:user)
    end

    it "should create a new instance given valid attributes" do
      user.should be_valid
    end

    it "should be invalid given a wrong password_confirmation" do
      user.password_confirmation = 'blub'
      user.should_not be_valid
      user.should have(1).error_on(:password)
    end

    it "should be invalid with a pasword of length less than 4" do
      user.password = 'sho'
      user.password_confirmation = 'sho'
      user.should_not be_valid
      user.should have(1).error_on(:password)
    end

    it "should be invalid if the name isn't unique" do
      user1 = Factory(:user, :name => 'peter')
      user2 = Factory.build(:user, :name => 'peter')
      user2.should_not be_valid
    end

    it "should be invalid if the email isn't unique" do
      user1 = Factory(:user, :email => 'peter@test.com')
      user2 = Factory.build(:user, :email => 'peter@test.com')
      user2.should_not be_valid
    end

    it "should be invalid given an invalid email address" do
      invalid_emails = ['foo@bar.', 'foo@bar.de.', 'foo.de.de', '@bar.de']
      invalid_emails.each do |email|
        user.email = email
        user.should_not be_valid
      end
    end
  end

  describe "to_json" do
    def forbidden_attributes
      @forbidden_attributes ||= [:encrypted_password, :ignored, :role, :salt, :email, :created_at, :sponsor]
    end

    def necessary_attributes
      @necessary_attributes ||= [:id, :singles_count, :time_zone, :wca, :name]
    end

    def user
      @user ||= Factory(:user, :name => 'peter', :email => 'peter@doc.com', :wca => '2007JDAE01')
    end

    def user_hash
      @user_hash ||= JSON.parse(user.to_json)
    end

    it "should not display sensible information via json" do
      forbidden_attributes.each do |attribute|
        user_hash['user'].keys.should_not include(attribute.to_s)
      end
    end

    it "should contain necessary informations about a user" do
      necessary_attributes.each do |attribute|
        user_hash['user'].keys.should include(attribute.to_s)
      end
    end

    it "should contain proper values" do
      user_hash['user']['name'].should == 'peter'
      user_hash['user']['singles_count'].should == 0
      user_hash['user']['wca'].should == '2007JDAE01'
    end
  end
end

describe User, "password" do

  def user
    @user ||= Factory(:user)
  end

  it "should flash the password after save" do
    user.password.should be_nil
    user.password_confirmation.should be_nil
  end

  it "should reset the password to a at least 12 characters long string"do
    user.reset_password!
    user.password.should =~ /[a-z0-9A-Z]{12}/
  end

end
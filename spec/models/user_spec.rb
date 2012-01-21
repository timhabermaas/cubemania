require "spec_helper"

describe User do

  describe "validations" do

    subject { Factory.build(:user) }

    it { should be_valid }

    it { should validate_presence_of(:email) }
    it { should validate_presence_of(:name) }
    it { should validate_presence_of(:role) }
    it { should validate_presence_of(:encrypted_password) }
    it { should validate_presence_of(:salt) }

    it { should_not allow_value('sho').for(:password) }
    it { should_not allow_value('u/rq').for(:password) }
    it { should_not allow_value('fagdjagladjklajfldsfklsdjlsd').for(:password) }

    it { should_not allow_value('sho').for(:email) }
    it { should_not allow_value('foo@bar.').for(:email) }
    it { should_not allow_value('foo.de.de').for(:email) }
    it { should_not allow_value('foo@bar.de.').for(:email) }
    it { should_not allow_value('@bar.').for(:email) }
    it { should allow_value('foo.hello@bar.de').for(:email) }

    it "should be invalid given a wrong password_confirmation" do
      subject.password = 'blubber'
      subject.password_confirmation = 'blub'
      subject.should_not be_valid
      subject.should have(1).error_on(:password)
    end

    it "should have a unique email" do
      user2 = Factory.create(:user, :email => subject.email)
      subject.should_not be_valid
      subject.should have(1).error_on(:email)
    end

    it "should have a unique name" do
      user2 = Factory.create(:user, :name => subject.name)
      subject.should_not be_valid
      subject.should have(1).error_on(:name)
    end
  end

  describe "to_json" do
    let(:forbidden_attributes) do
      [:encrypted_password, :ignored, :role, :salt, :email, :created_at, :sponsor]
    end

    let(:necessary_attributes) do
      [:id, :singles_count, :time_zone, :wca, :name]
    end

    let(:user) do
      Factory(:user, :name => 'peter', :email => 'peter@doc.com', :wca => '2007JDAE01')
    end

    let(:user_hash) do
      JSON.parse(user.to_json)
    end

    it "should not display sensible information via json" do
      forbidden_attributes.each do |attribute|
        user_hash.keys.should_not include(attribute.to_s)
      end
    end

    it "should contain necessary informations about a user" do
      necessary_attributes.each do |attribute|
        user_hash.keys.should include(attribute.to_s)
      end
    end

    it "should contain proper values" do
      user_hash['name'].should == 'peter'
      user_hash['singles_count'].should == 0
      user_hash['wca'].should == '2007JDAE01'
    end
  end

  describe "password" do
    let(:user) do
      Factory(:user)
    end

    it "should flash the password after save" do
      user.password.should be_nil
      user.password_confirmation.should be_nil
    end

    it "should reset the password to a new one" do
      old_password = user.encrypted_password
      user.reset_password!.should =~ /[a-z0-9A-Z]{12}/
      old_password.should_not == user.encrypted_password
    end
  end

  describe "#singles.best" do
    let(:user) { Factory(:user) }
    let(:puzzle) { Factory(:puzzle) }

    subject { user.singles.best(puzzle).time }

    before do
      Factory(:single, :time => 20, :user => user, :puzzle => puzzle)
      Factory(:single, :time => 9, :user => user, :puzzle => puzzle, :penalty => "dnf")
      Factory(:single, :time => 10, :user => user, :puzzle => puzzle)
      Factory(:single, :time => 30, :user => user, :puzzle => puzzle)
    end

    it { should == 10 }
  end
end
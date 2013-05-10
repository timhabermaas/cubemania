require "spec_helper"

describe User do

  describe "validations" do

    subject { build(:user) }

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
      user2 = create(:user, :email => subject.email)
      subject.should_not be_valid
      subject.should have(1).error_on(:email)
    end

    it "should have a unique name" do
      user2 = create(:user, :name => subject.name)
      subject.should_not be_valid
      subject.should have(1).error_on(:name)
    end
  end

  describe "password" do
    let(:user) do
      create(:user)
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

  describe "#follow!" do
    let(:peter) { create :user, :name => "Peter" }
    let(:nick) { create :user, :name => "Nick" }

    it "follows peter" do
      nick.follow!(peter)
      expect(peter.followers).to eq [nick]
      expect(nick.followees).to eq [peter]
    end

    it "doesn't follow people twice" do
      nick.follow!(peter)
      nick.follow!(peter)
      expect(peter.followers).to eq [nick]
      expect(nick.followees).to eq [peter]
    end
  end

  describe "#unfollow!" do
    let(:peter) { create :user, :name => "Peter" }
    let(:nick) { create :user, :name => "Nick" }

    it "successfully unfollows users" do
      nick.follow!(peter)
      nick.unfollow!(peter)
      expect(nick.followees).to eq([])
      expect(peter.followers).to eq([])
    end

    it "doesn't crash if there's no following relationship" do
      nick.unfollow!(peter)
      expect(nick.followees).to eq([])
      expect(peter.followers).to eq([])
    end
  end

  describe ".authorize" do
    let!(:user) { create :user, :name => "charlie", :password => "password",
                                                    :password_confirmation => "password" }
    it "returns user if name and password match" do
      User.authorize("charlie", "password").should == user
    end

    it "returns nil if there's no match" do
      User.authorize("charlie", "test").should be_nil
    end

    it "ignores case" do
      User.authorize("Charlie", "password").should == user
    end
  end
end

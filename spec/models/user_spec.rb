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

  describe "#singles.best" do
    let(:user) { create(:user) }
    let(:puzzle) { create(:puzzle) }

    subject { user.singles.best(puzzle).time }

    before do
      create(:single, :time => 20, :user => user, :puzzle => puzzle)
      create(:single, :time => 9, :user => user, :puzzle => puzzle, :penalty => "dnf")
      create(:single, :time => 10, :user => user, :puzzle => puzzle)
      create(:single, :time => 30, :user => user, :puzzle => puzzle)
    end

    it { should == 10 }
  end


  describe "#best_average" do
    let(:user) { create(:user) }
    let(:puzzle) { create(:puzzle) }

    context "given [10, 3, 15, 4, 6, 8, 7]" do
      before :each do
        [10, 3, 15, 4, 6, 8, 7].each do |time|
          create :single, :puzzle => puzzle, :user => user, :time => time
        end
      end

      it "returns an average with a time of 6" do
        user.best_average(puzzle, 5).time.should == (4 + 6 + 8)/3.0
      end
    end

    context "given [10, 3, DNF, 5, DNF, 2]" do
      before :each do
        singles = [10, 3, 20, 5, 4, 2].map do |time|
          create :single, :puzzle => puzzle, :user => user, :time => time
        end
        singles[2].toggle_dnf!
        singles[4].toggle_dnf!
      end

      it "returns a dnf average" do
        user.best_average(puzzle, 5).should be_dnf
      end
    end
  end

  describe "follow!" do
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

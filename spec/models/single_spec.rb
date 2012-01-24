require "spec_helper"

describe Single do
  let(:single) { Factory.build(:single, :user => user, :puzzle => puzzle, :time => 20000) }
  let(:user) { Factory(:user) }
  let(:puzzle) { Factory(:puzzle) }

  subject { single }

  describe "validations" do
    it { should be_valid }
    it { should validate_presence_of(:time) }
    it { should validate_presence_of(:user_id) }
    it { should validate_presence_of(:puzzle_id) }
    it { should allow_value("dnf").for(:penalty) }
    it { should allow_value("plus2").for(:penalty) }
    it { should_not allow_value("m").for(:penalty) }
  end

  it { should_not allow_mass_assignment_of(:user_id) }

  it { should respond_to(:human_time) }

  it "sets penalty to nil before validation if it's left blank" do
    single = Single.new :penalty => " "
    single.valid?
    single.penalty.should == nil
  end

  describe "#toggle_dnf" do
    subject { single.dnf? }

    context "when no penalty" do
      before { single.penalty = nil }
      it "should become true" do
        single.toggle_dnf!
        should == true
      end
    end

    context "when plus2" do
      before { single.penalty = "plus2" }
      it "should become true and decrease time" do
        single.toggle_dnf!
        should == true
        single.time.should == 18000
      end
    end

    context "when dnf" do
      before { single.penalty = "dnf" }
      it "should become false" do
        single.toggle_dnf!
        should == false
      end
    end
  end

  describe "#toggle_plus2" do
    subject { single.plus2? }

    context "when no penalty" do
      before { single.penalty = nil }
      it "should increase time and update flag" do
        single.toggle_plus2!
        should == true
        single.time.should == 22000
      end
    end

    context "when dnf" do
      before { single.penalty = "dnf" }
      it "should increase time and update flag" do
        single.toggle_plus2!
        should == true
        single.time.should == 22000
      end
    end

    context "when plus2" do
      before { single.penalty = "plus2" }
      it "should decrease time and update flag" do
        single.toggle_plus2!
        should == false
        single.time.should == 18000
      end
    end
  end

  describe "#set_time" do
    subject { single.time }

    context "when human_time is set to non blank string" do
      before { single.human_time = "1:12.32" }
      it "should set time to 72320 for 1:12.32" do
        should == 72320
      end

      it "should not be overriden by blank time" do
        single.time = ""
        should == 72320
      end

      it "should allow time to be reset to nil" do
        single.time = nil
        should == nil
      end
    end

    context "when human_time is set to blank string" do
      it "should not change old time to 0" do
        single.time = 1337
        single.human_time = ""
        should == 1337
      end
    end
  end
end

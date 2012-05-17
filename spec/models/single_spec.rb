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

  it "sets penalty to nil before validation if it's left blank" do
    single = Single.new :penalty => " "
    single.valid?
    single.penalty.should == nil
  end

  it "is destroyable if it doesn't belong to an average" do
    single = create :single
    lambda {
      single.destroy
    }.should change(Single, :count).by(-1)
  end

  it "isn't destroyable if it belongs to an average" do
    average = create :average, :singles => create_list(:single, 5)
    average.singles.first.destroy
    average.singles.first.destroy.should == false
    average.singles.count.should == 5
  end

  describe "updating comments" do
    let(:record) { double :record }

    it "triggers comment updating when changing comment" do
      single = create :single

      single.stub(:records) { [record, record] }
      record.should_receive(:update_comment!).twice
      single.update_attributes(:comment => "OMG! Awesome solve!")
    end

    it "doesn't trigger comment updating when comment didn't change" do
      single = create :single
      single.stub(:records) { [record] }
      record.should_not_receive(:update_comment!)
      single.update_attributes(:penalty => "dnf")
    end
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
end

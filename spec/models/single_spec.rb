require 'spec_helper'

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

  it { should belong_to(:user) }
  it { should belong_to(:puzzle) }

  it { should_not allow_mass_assignment_of(:user_id) }

  it { should respond_to(:human_time) }

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

    context "when human_time is set" do
      before do
        single.human_time = "1:12.32"
        single.save
      end

      it "should set time to 72320 for 1:12.32" do
        should == 72320
      end
    end

    context "when human_time is not set" do
      before do
        single.human_time = ""
        single.time = 1337
        single.save
      end

      it "should not change time" do
        should == 1337
      end
    end
  end

  describe "update single record" do
    subject { record.first.try(:time) }

    let(:record) { Record.where(:amount => 1, :puzzle_id => puzzle.id, :user_id => user.id) }

    before do
      Single.delete_all
      Record.delete_all
      @fastest = Factory(:single, :time => 10, :user => user, :puzzle => puzzle)
    end

    context "when creating the first single" do
      it { should == @fastest.time }
    end

    context "when deleting the fastest single" do
      before do
        @new_single = Factory(:single, :time => 9, :user => user, :puzzle => puzzle)
      end

      it { should == @new_single.time }
      it { record.first.singles.first.should == @new_single }
    end

    context "when creating worse singles" do
      before do
        Factory(:single, :time => 20, :user => user, :puzzle => puzzle)
        Factory(:single, :time => 30, :user => user, :puzzle => puzzle)
      end

      it { should == @fastest.time }
    end

    context "when deleting a single" do
      before do
        Factory(:single, :time => 20, :user => user, :puzzle => puzzle)
        single = Factory(:single, :time => 5, :user => user, :puzzle => puzzle)
        single.destroy
      end

      it { should == @fastest.time }
    end

    context "when deleting all singles" do
      before do
        @fastest.destroy
      end

      it { should == nil }
    end

    context "when dnfing the last single" do
      before do
        Factory(:single, :time => 20, :user => user, :puzzle => puzzle)
        single = Factory(:single, :time => 5, :user => user, :puzzle => puzzle)
        single.update_attribute(:penalty, "dnf")
      end

      it { should == @fastest.time }
    end

    context "when there's no record but existing singles" do
      before do
        Single.delete_all
        @single = Factory(:single, :time => 30, :user => user, :puzzle => puzzle)
        Factory(:single, :time => 40, :user => user, :puzzle => puzzle)
        Record.delete_all
        @single.destroy
      end

      it { should == 40 }
    end
  end

  describe "update average of 5 record" do

    def avg5_record
      user.records.for(puzzle.id, 5)
    end

    before do
      4.downto(1) do |n|
        Factory(:single, :user => user, :puzzle => puzzle, :time => n)
      end # 4 3 2 1
    end

    context "when not enough times are available" do
      it "shouldn't set a record" do
        avg5_record.should be_nil
      end
    end

    context "when adding singles" do
      it "should change average of 5 from 3 to 2 and update ids" do
        Factory(:single, :user => user, :puzzle => puzzle, :time => 5) # 4 3 2 1 5
        s = Factory(:single, :user => user, :puzzle => puzzle, :time => 1) # 4 3 2 1 5 1
        avg5_record.time.should == 2
        avg5_record.singles.map(&:id).should include(s.id)
      end
    end

    context "when updating singles"

    context "when deleting singles"
  end
end

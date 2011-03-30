require 'spec_helper'

describe Single do
  let(:single) { Factory.build(:single) }
  subject { single }

  it { should be_valid }

  it { should validate_presence_of(:time) }
  it { should validate_presence_of(:user_id) }
  it { should validate_presence_of(:puzzle_id) }

  it { should belong_to(:user) }
  it { should belong_to(:puzzle) }

  it { should_not allow_mass_assignment_of(:user_id) }

  it { should respond_to(:human_time) }

  describe "#toggle_dnf" do
    subject { single.dnf? }

    context "when dnf = false" do
      before { single.dnf = false }
      it "should become true" do
        single.toggle_dnf!
        should == true
      end
    end

    context "when dnf = true" do
      before { single.dnf = true }
      it "should become false" do
        single.toggle_dnf!
        should == false
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
end

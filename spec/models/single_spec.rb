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
end

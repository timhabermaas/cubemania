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

  describe "#update_single_record" do
    subject { Record.where(:amount => 1).first.try(:time) }

    let(:record) do
      Record.where(:amount => 1).first
    end

    before do
      Single.delete_all
      Record.delete_all
      @fastest = Factory(:single, :time => 10)
    end

    context "when creating the first single" do
      it { should == @fastest.time }
    end

    context "when creating worse singles" do
      before do
        Factory(:single, :time => 20)
        Factory(:single, :time => 30)
      end

      it { should == @fastest.time }
    end

    context "when deleting a single" do
      before do
        Factory(:single, :time => 20)
        single = Factory(:single, :time => 5)
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
        Factory(:single, :time => 20)
        single = Factory(:single, :time => 5)
        single.update_attribute(:dnf, true)
      end

      it { should == @fastest.time }
    end
  end
end

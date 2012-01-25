require 'update_recent_records'

class Single; end
class CubingAverage; end
class Record; end

describe UpdateRecentRecords do
  subject { UpdateRecentRecords }
  let(:user) { stub(:id => 2) }
  let(:puzzle) { stub(:id => 3) }

  describe ".for" do
    it "calls for_amount for 1, 5 and 12" do
      subject.should_receive(:for_amount).with(user, puzzle, 1)
      subject.should_receive(:for_amount).with(user, puzzle, 5)
      subject.should_receive(:for_amount).with(user, puzzle, 12)
      subject.for(user, puzzle)
    end
  end

  describe ".for_amount" do
    context "not enough singles given" do
      before do
        Single.stub_chain(:for_user_and_puzzle, :recent) { [] }
      end

      it "updates nothing" do
        Record.should_not_receive(:update_with!)
        UpdateRecentRecords.for_amount(user, puzzle, 1)
        UpdateRecentRecords.for_amount(user, puzzle, 5)
        UpdateRecentRecords.for_amount(user, puzzle, 12)
      end
    end

    context "five singles given" do
      let(:singles) { [stub]*5 }

      before do
        Single.stub_chain(:for_user_and_puzzle, :recent).with(5) { singles }
      end

      it "updates avg5 record if average is valid" do
        average = stub(:time => 10, :dnf? => false)
        CubingAverage.should_receive(:new).with(singles).and_return(average)
        Record.should_receive(:update_with!).with(user, puzzle, 5, 10, singles)
        UpdateRecentRecords.for_amount(user, puzzle, 5)
      end

      it "does not update avg5 record if average is invalid" do
        average = stub(:time => 10, :dnf? => true)
        CubingAverage.should_receive(:new).with(singles).and_return(average)
        Record.should_not_receive(:update_with!)
        UpdateRecentRecords.for_amount(user, puzzle, 5)
      end
    end

    context "single" do
      let(:singles) { [stub] }

      before do
        Single.stub_chain(:for_user_and_puzzle, :recent).with(1) { singles }
      end

      it "updates single record" do
        average = stub(:time => 1337, :dnf? => false)
        CubingAverage.should_receive(:new).with(singles).and_return(average)
        Record.should_receive(:update_with!).with(user, puzzle, 1, 1337, singles)
        UpdateRecentRecords.for_amount(user, puzzle, 1)
      end
    end
  end
end

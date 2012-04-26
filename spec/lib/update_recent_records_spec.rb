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

    it "returns trueish value if at least one record was updated" do
      subject.stub(:for_amount).with(user, puzzle, 1)  { false }
      subject.stub(:for_amount).with(user, puzzle, 5)  { true }
      subject.stub(:for_amount).with(user, puzzle, 12) { false }
      subject.for(user, puzzle).should be_true
    end

    it "returns falseish value if no record was updated" do
      subject.stub(:for_amount) { false }
      subject.for(user, puzzle).should be_false
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

      it "returns falseish value" do
        UpdateRecentRecords.for_amount(user, puzzle, 1).should be_false
        UpdateRecentRecords.for_amount(user, puzzle, 5).should be_false
        UpdateRecentRecords.for_amount(user, puzzle, 12).should be_false
      end
    end

    context "five singles given" do
      let(:singles) { [stub]*5 }

      before do
        Single.stub_chain(:for_user_and_puzzle, :recent).with(5) { singles }
      end

      context "and average is valid" do
        let(:average) { stub(:time => 10, :dnf? => false) }

        it "updates avg5 record" do
          CubingAverage.should_receive(:new).with(singles).and_return(average)
          Record.should_receive(:update_with!).with(user, puzzle, 5, 10, singles)
          UpdateRecentRecords.for_amount(user, puzzle, 5)
        end

        it "returns trueish value" do
          CubingAverage.stub(:new) { average }
          Record.stub(:update_with!) { true }
          UpdateRecentRecords.for_amount(user, puzzle, 5).should be_true
        end
      end

      context "and average is invalid" do
        let(:average) { stub(:time => 10, :dnf? => true) }

        it "does not update avg5 record" do
          CubingAverage.should_receive(:new).with(singles).and_return(average)
          Record.should_not_receive(:update_with!)
          UpdateRecentRecords.for_amount(user, puzzle, 5)
        end

        it "returns falseish value" do
          CubingAverage.stub(:new) { average }
          UpdateRecentRecords.for_amount(user, puzzle, 5).should be_false
        end
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

require "update_recent_records"
require_relative "../../app/models/record_type"

class Single; end
class CubingAverage; end
class Record; end

describe UpdateRecentRecords do
  subject { UpdateRecentRecords }
  let(:user) { stub(:id => 2) }
  let(:puzzle) { stub(:id => 3) }

  describe ".for" do
    it "calls for_amount for all types" do
      subject.should_receive(:for_amount).with(user, puzzle, RecordType.all[0])
      subject.should_receive(:for_amount).with(user, puzzle, RecordType.all[1])
      subject.should_receive(:for_amount).with(user, puzzle, RecordType.all[2])
      subject.for(user, puzzle)
    end

    it "returns trueish value if at least one record was updated" do
      subject.stub(:for_amount).with(user, puzzle, RecordType.all[0])  { false }
      subject.stub(:for_amount).with(user, puzzle, RecordType.all[1])  { true }
      subject.stub(:for_amount).with(user, puzzle, RecordType.all[2]) { false }
      subject.for(user, puzzle).should be_true
    end

    it "returns falseish value if no record was updated" do
      subject.stub(:for_amount) { false }
      subject.for(user, puzzle).should be_false
    end
  end

  describe ".for_amount" do
    let(:single_type) { stub(:single_type, :count => 1) }
    let(:avg5_type) { stub(:avg5_type, :count => 5) }
    let(:avg12_type) { stub(:avg12_type, :count => 12) }

    context "not enough singles given" do
      before do
        Single.stub_chain(:for_user_and_puzzle, :recent) { [] }
      end

      it "updates nothing" do
        Record.should_not_receive(:update_with!)
        UpdateRecentRecords.for_amount(user, puzzle, single_type)
        UpdateRecentRecords.for_amount(user, puzzle, avg5_type)
        UpdateRecentRecords.for_amount(user, puzzle, avg12_type)
      end

      it "returns falseish value" do
        UpdateRecentRecords.for_amount(user, puzzle, single_type).should be_false
        UpdateRecentRecords.for_amount(user, puzzle, avg5_type).should be_false
        UpdateRecentRecords.for_amount(user, puzzle, avg12_type).should be_false
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
          UpdateRecentRecords.for_amount(user, puzzle, avg5_type)
        end

        it "returns trueish value" do
          CubingAverage.stub(:new) { average }
          Record.stub(:update_with!) { true }
          UpdateRecentRecords.for_amount(user, puzzle, avg5_type).should be_true
        end
      end

      context "and average is invalid" do
        let(:average) { stub(:time => 10, :dnf? => true) }

        it "does not update avg5 record" do
          CubingAverage.should_receive(:new).with(singles).and_return(average)
          Record.should_not_receive(:update_with!)
          UpdateRecentRecords.for_amount(user, puzzle, avg5_type)
        end

        it "returns falseish value" do
          CubingAverage.stub(:new) { average }
          UpdateRecentRecords.for_amount(user, puzzle, avg5_type).should be_false
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
        UpdateRecentRecords.for_amount(user, puzzle, single_type)
      end
    end
  end
end

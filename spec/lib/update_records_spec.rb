require "update_records"

class Record; end

describe UpdateRecords do
  subject { UpdateRecords }
  let(:user) { stub(:id => 2) }
  let(:puzzle) { stub(:id => 3) }

  describe ".for" do
    it "calls for_amount for 1, 5 and 12" do
      subject.should_receive(:single).with(user, puzzle)
      subject.should_receive(:for_amount).with(user, puzzle, 5)
      subject.should_receive(:for_amount).with(user, puzzle, 12)
      subject.for(user, puzzle)
    end
  end

  describe ".single" do
    context "best single found" do
      let(:best) { stub(:time => 1337) }

      it "sets the single record to the lowest time" do
        user.stub_chain(:singles, :best) { best }
        Record.should_receive(:update_with!).with(user, puzzle, 1, 1337, [best])
        subject.single(user, puzzle)
      end
    end

    context "best single not found" do
      let(:best) { nil }

      it "sets the single record to the lowest time" do
        user.stub_chain(:singles, :best) { best }
        Record.should_not_receive(:update_with!)
        Record.should_receive(:remove!).with(user, puzzle, 1)
        subject.single(user, puzzle)
      end
    end
  end

  describe ".for_amount" do
    it "updates record with best average" do
      singles = [stub, stub, stub, stub, stub]
      average = stub(:time => 12341, :singles => singles, :dnf? => false)
      user.should_receive(:best_average).with(puzzle, 5) { average }
      Record.should_receive(:update_with!).with(user, puzzle, 5, 12341, singles)
      subject.for_amount(user, puzzle, 5)
    end

    it "deletes any existing record if the current best average is a dnf" do
      average = stub(:time => nil, :dnf? => true)
      user.should_receive(:best_average).with(puzzle, 5) { average }
      Record.should_not_receive(:update_with)
      Record.should_receive(:remove!).with(user, puzzle, 5)
      subject.for_amount(user, puzzle, 5)
    end
  end
end

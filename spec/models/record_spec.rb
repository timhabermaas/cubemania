require 'spec_helper'

describe Record do
  describe "validations" do
    it { should validate_presence_of :puzzle_id }
    it { should validate_presence_of :user_id }
    it { should validate_presence_of :time }
    it { should validate_presence_of :amount }
    it { should validate_presence_of :singles }
    it { should validate_presence_of :set_at }

    it "has as many singles as amount" do
      record = Record.new :amount => 5, :singles => [Single.new] * 3
      record.should_not be_valid
      record.errors[:singles].should_not be_empty
    end
  end

  describe "#set_at" do
    it "is set to date of most recent single" do
      s = []
      s << create(:single, :created_at => Time.new(2012, 3, 1))
      s << create(:single, :created_at => Time.new(2012, 3, 4))
      s << create(:single, :created_at => Time.new(2012, 1, 8))
      s << create(:single, :created_at => Time.new(2012, 5, 2))
      s << create(:single, :created_at => Time.new(2012, 2, 2))
      record = Record.new :amount => 5, :singles => s
      record.save
      record.set_at.should == s[3].created_at
    end
  end

  describe "#singles" do
    it "defaults to an empty array" do
      subject.singles.should == []
    end

    it "orders them by singles.created_at" do
      single_old = create :single, :created_at => DateTime.new(2010, 1, 2)
      single_new = create :single, :created_at => DateTime.new(2011, 1, 2)
      single_today = create :single
      record = create :record, :singles => [single_new] + [single_today] * 3 + [single_old]
      record.singles.ordered.last.should == single_today
      record.singles.ordered.first.should == single_old
    end
  end

  describe ".grouped_by_puzzle_and_amount" do
    let(:puzzle_1) { create :puzzle }
    let(:puzzle_2) { create :puzzle }
    let!(:record_1) { create :record, :amount => 5, :puzzle => puzzle_1 }
    let!(:record_2) { create :record, :amount => 5, :puzzle => puzzle_2 }
    let!(:record_3) { create :record, :amount => 12, :puzzle => puzzle_2 }

    let(:records) { Record.grouped_by_puzzle_and_amount }

    it "groups all records by puzzle and then by amount" do
      expect(records[puzzle_1][5]).to eq(record_1)
      expect(records[puzzle_2][5]).to eq(record_2)
      expect(records[puzzle_2][12]).to eq(record_3)
    end
  end

  describe "comments" do
    let(:single_1) { create :single, :comment => "foo" }
    let(:single_2) { create :single, :comment => "muh" }
    let(:single_3) { create :single, :comment => "too long"*30 }
    let(:record) { create :record, :singles => create_list(:single, 3) + [single_1] + [single_2] }

    subject { record.comment }

    it "concatenates comments from singles" do
      should == "foo; muh"
    end

    it "cuts off too long comments without failing comment validation" do
      record = create :record, :singles => [single_1, single_2, single_3, single_1, single_2]
      record.comment.size.should be <= 255
    end

    describe "#update_comment!" do
      let(:singles) { create_list :single, 5 }

      it "fetches comments from singles and updates record object" do
        record = create :record, :singles => singles
        singles[0].should_receive(:comment).at_least(:once) { "bar" }
        singles[1].should_receive(:comment).at_least(:once) { "foo" }
        record.should_receive(:update_attributes).with(:comment => "bar; foo")
        record.update_comment!
      end
    end
  end

  describe ".update_with" do
    let(:singles) { create_list(:single, 5) }

    context "when record exists" do
      let!(:record) { create :record, :time => 10, :amount => 5, :singles => singles }
      let(:user) { record.user }
      let(:puzzle) { record.puzzle }

      context "and the new time is faster" do
        it "updates the existing record" do
          Record.update_with!(user, puzzle, 5, 9, singles)
          record.reload.time.should == 9
        end

        it "returns trueish value" do
          Record.update_with!(user, puzzle, 5, 9, singles).should be_true
        end
      end

      context "and the new time isn't faster" do
        it "doesn't update the record" do
          Record.update_with!(user, puzzle, 5, 11, singles)
          record.reload.time.should == 10
        end

        it "returns falseish value" do
          Record.update_with!(user, puzzle, 5, 11, singles).should be_false
        end
      end

      context "force writing of record" do
        it "overwrites existing record if force flag is set" do
          Record.update_with!(user, puzzle, 5, 11, singles, true)
          record.reload.time.should == 11
        end

        it "returns trueish value" do
          Record.update_with!(user, puzzle, 5, 11, singles, true).should be_true
        end
      end
    end

    context "when no record exists" do
      it "creates a new record" do
        user = stub(:id => 4)
        puzzle = stub(:id => 4)
        lambda {
          Record.update_with!(user, puzzle, 5, 9, singles)
        }.should change(Record, :count).by(1)
      end
    end
  end
end

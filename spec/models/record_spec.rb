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

  describe ".recent" do
    let!(:record_1) { create :record, :time => 9, :user_id => 2, :puzzle_id => 3, :amount => 5 }
    let!(:record_2) { create :record, :time => 5, :user_id => 2, :puzzle_id => 3, :amount => 5 }
    let!(:record_3) { create :record, :time => 11, :user_id => 3, :puzzle_id => 3, :amount => 5 }
    let!(:record_4) { create :record, :time => 4, :user_id => 2, :puzzle_id => 3, :amount => 12 }

    it "returns just the recent records for each user" do
      result = Record.where(:amount => 5).recent
      expect(result).to have(2).items
      expect(result).to include(record_2)
      expect(result).to include(record_3)
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
    let(:single_1) { stub(:single, :id => 1, :created_at => DateTime.new(2013)) }
    let(:single_2) { stub(:single, :id => 1, :created_at => DateTime.new(2014)) }
    let(:single_3) { stub(:single, :id => 1, :created_at => DateTime.new(2012)) }
    let(:singles) { [single_1, single_2, single_3] }

    it "defaults to an empty array" do
      subject.singles.should == []
    end

    it "ensures a natural order of single" do
      r = Record.new :singles => singles
      expect(r.singles).to eq([single_3, single_1, single_2])
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

  describe ".younger_than" do
    let!(:record_1) { create :record, :user_id => 2, :puzzle_id => 3, :set_at => DateTime.new(2013, 4, 5) }
    let!(:record_2) { create :record, :user_id => 2, :puzzle_id => 4, :set_at => DateTime.new(2013, 4, 7) }
    let!(:record_3) { create :record, :user_id => 2, :puzzle_id => 3, :set_at => DateTime.new(2013, 4, 4) }

    it "returns all records which could" do
      single = stub(:single,
                    :created_at => DateTime.new(2013, 4, 5),
                    :user_id => 2,
                    :puzzle_id => 3)

      records = Record.younger_than(single)
      expect(records).to have(2).items
      expect(records).to include(record_1)
      expect(records).to include(record_3)
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

  describe ".build_from_singles_and_type_and_time" do
    let(:type) { stub(:type, :count => 5) }
    let(:singles) { [mock_model("Single", :user_id => 12, :puzzle_id => 13)] * 5 }

    it "returns a new record filled with attributes from singles" do
      record = Record.build_from_singles_and_type_and_time(singles, type, 141)
      expect(record.time).to eq(141)
      expect(record.singles).to eq(singles)
      expect(record.user_id).to eq(12)
      expect(record.amount).to eq(5)
      expect(record.puzzle_id).to eq(13)
    end
  end
end

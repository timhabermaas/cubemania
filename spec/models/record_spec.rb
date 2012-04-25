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
    it "is set to date of last single" do
      last_single = nil
      last_single = create :single
      record = Record.new :amount => 5, :singles => ([create :single] * 4) + [last_single]
      record.save
      record.set_at.should == last_single.created_at
    end
  end

  describe "#singles" do
    it "defaults to an empty array" do
      subject.singles.should == []
    end

    it "orders them by singles.created_at" do
      single_old = single_new = nil
      Timecop.freeze(DateTime.new(2010, 1, 2)) do
        single_old = create :single
      end
      Timecop.freeze(DateTime.new(2011, 1, 2)) do
        single_new = create :single
      end
      single_today = create :single
      record = create :record, :singles => [single_new] + [single_today] * 3 + [single_old]
      record.singles.ordered.last.should == single_today
      record.singles.ordered.first.should == single_old
    end
  end

  describe "#comment" do
    let(:single_1) { create :single, :comment => "foo" }
    let(:single_2) { create :single, :comment => "muh" }
    let(:single_3) { create :single, :comment => "too long"*100 }
    let(:record) { create :record, :singles => create_list(:single, 3) + [single_1] + [single_2] }

    subject { record.comment }

    it "concatenates comments from singles" do
      should == "foo; muh"
    end

    it "cuts off too long comments without failing comment validation" do
      record = create :record, :singles => create_list(:single, 4) + [single_3]
      record.comment.size.should == 255
    end
  end

  describe "#cubing_average" do
    let(:record) { create :record }
    it "returns an instance of CubingAverage filled with proper stuff" do
      average = record.cubing_average
      average.should be_instance_of CubingAverage
      average.singles.should == record.singles
    end
  end

  describe ".update_with" do
    let(:singles) { create_list(:single, 5) }

    context "existing record" do
      let!(:record) { create :record, :time => 10, :amount => 5, :singles => singles }
      let(:user) { record.user }
      let(:puzzle) { record.puzzle }

      it "updates the existing record with faster time" do
        Record.update_with!(user, puzzle, 5, 9, singles)
        record.reload.time.should == 9
      end

      it "doesn't update the existing record if the time is not faster" do
        Record.update_with!(user, puzzle, 5, 11, singles)
        record.reload.time.should == 10
      end

      context "force writing of record" do
        it "overwrites existing record if force flag is set" do
          Record.update_with!(user, puzzle, 5, 11, singles, true)
          record.reload.time.should == 11
        end
      end
    end

    context "no existing record" do
      it "creates a new record" do
        user = stub(:id => 4)
        puzzle = stub(:id => 4)
        lambda {
          Record.update_with!(user, puzzle, 5, 9, singles)
        }.should change(Record, :count).by(1)
      end
    end
  end

  describe ".remove!" do
    let(:user) { create :user }
    let(:puzzle_1) { create :puzzle }
    let(:puzzle_2) { create :puzzle }

    before :each do
      create :record, :user => user, :puzzle => puzzle_1, :amount => 1, :singles => build_list(:single, 1)
      @record = create :record, :user => user, :puzzle => puzzle_1, :amount => 5, :singles => build_list(:single, 5)
      create :record, :user => user, :puzzle => puzzle_1, :amount => 12, :singles => build_list(:single, 12)
      create :record, :user => user, :puzzle => puzzle_2, :amount => 5, :singles => build_list(:single, 5)
    end

    it "removes the existing avg5 record for puzzle 2 and user 1" do
      lambda {
        Record.remove!(user, puzzle_1, 5)
      }.should change(Record, :count).by(-1)
      Record.exists?(@record).should == false
    end

    it "does nothing if the record doesn't exist" do
      lambda {
        Record.remove!(user, puzzle_2, 12)
      }.should_not change(Record, :count)
    end
  end
end

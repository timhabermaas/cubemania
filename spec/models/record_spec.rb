require 'spec_helper'

describe Record do
  describe "validations" do
    it { should validate_presence_of :puzzle_id }
    it { should validate_presence_of :user_id }
    it { should validate_presence_of :time }
    it { should validate_presence_of :amount }
    it { should validate_presence_of :singles }

    it "has as many singles as amount" do
      record = Record.new :amount => 5, :singles => [Single.new] * 3
      record.should_not be_valid
      record.errors[:singles].should_not be_empty
    end
  end

  describe "#singles" do
    it "defaults to an empty array" do
      subject.singles.should == []
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
        old_updated_at = record.updated_at
        sleep 5
        Record.update_with!(user, puzzle, 5, 10, singles)
        record.reload.updated_at.should == old_updated_at
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

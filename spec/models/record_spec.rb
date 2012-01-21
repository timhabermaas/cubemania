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
end

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
    before :each do
      Single.any_instance.stub(:update_records_after_create) # FIXME dependency :(
    end

    let(:user) { create :user }
    let(:puzzle) { create :puzzle }
    let(:singles) { create_list(:single, 5, :user => user, :puzzle => puzzle) }

    context "existing record" do
      let!(:record) { create :record, :time => 10, :user => user, :puzzle => puzzle, :amount => 5, :singles => singles }

      it "updates the existing record with faster time" do
        Record.update_with!(user, puzzle, 5, 9, singles)
        record.reload.time.should == 9
      end

      it "doesn't update the existing record if the time is not faster" do
        old_updated_at = record.updated_at
        Record.update_with!(user, puzzle, 5, 10, singles)
        record.reload.updated_at.should == old_updated_at
      end
    end

    context "no existing record" do
      it "creates a new record" do
        lambda {
          Record.update_with!(user, puzzle, 5, 9, singles)
        }.should change(Record, :count).by(1)
      end
    end
  end
end

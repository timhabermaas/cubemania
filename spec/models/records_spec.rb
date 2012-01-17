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
end

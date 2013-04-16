require "spec_helper"

describe RecordActivity do
  describe "#corrupt" do
    let(:record) { create :record, :updated_at => DateTime.new(2013, 1, 2) }

    it "can be not corrupt" do
      ra = create :record_activity, :trackable => record, :created_at => DateTime.new(2013, 1, 3)
      expect(ra).to_not be_corrupt
    end

    it "is corrupt if record is nil" do
      expect(RecordActivity.new(:trackable => nil)).to be_corrupt
    end

    it "is corrupt if record was updated after activity was created" do
      ra = create :record_activity, :trackable => record, :created_at => DateTime.new(2013, 1, 1)
      expect(ra).to be_corrupt
    end
  end
end

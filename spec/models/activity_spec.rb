require "spec_helper"

describe Activity do
  describe "#corrupt?" do
    it "is corrupt if trackable isn't present" do
      expect(Activity.new(:trackable => nil)).to be_corrupt
    end
  end
end

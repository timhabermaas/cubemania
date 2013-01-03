require "spec_helper"

describe ApplicationHelper do
  describe "#possessive" do
    it "adds an 's to Peter" do
      helper.possessive("Peter").should == "Peter's"
    end

    it "only adds an ' to Klaus" do
      helper.possessive("Klaus").should == "Klaus'"
    end
  end

  describe "#compare" do
    it "returns 'faster' if my time is faster than the other time" do
      expect(helper.compare(140, 200)).to eq "faster"
    end

    it "returns 'faster' if the other time doesn't exist" do
      expect(helper.compare(140, nil)).to eq "faster"
    end

    it "returns 'slower' if my time is slower than the other time" do
      expect(helper.compare(200, 123)).to eq "slower"
    end

    it "returns 'slower' if my time doesn't exist" do
      expect(helper.compare(nil, 123)).to eq "slower"
    end

    it "returns 'equal' if both times are the same" do
      expect(helper.compare(412, 412)).to eq "equal"
    end

    it "returns 'equal' if both times don't exist" do
      expect(helper.compare(nil, nil)).to eq "equal"
    end
  end
end

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
end

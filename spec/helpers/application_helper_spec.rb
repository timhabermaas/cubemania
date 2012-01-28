require "spec_helper"

describe ApplicationHelper do
  describe "#s" do
    it "should set a proper sign" do
      helper.s(10).should == "+10"
      helper.s(-10).should == "-10"
      helper.s(0).should == "0"
    end
  end

  describe "#possessive" do
    it "adds an 's to Peter" do
      helper.possessive("Peter").should == "Peter's"
    end

    it "only adds an ' to Klaus" do
      helper.possessive("Klaus").should == "Klaus'"
    end
  end
end

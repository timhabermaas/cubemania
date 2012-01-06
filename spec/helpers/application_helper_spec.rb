require "spec_helper"

describe ApplicationHelper do
  describe "#s" do
    it "should set a proper sign" do
      helper.s(10).should == "+10"
      helper.s(-10).should == "-10"
      helper.s(0).should == "0"
    end
  end
end

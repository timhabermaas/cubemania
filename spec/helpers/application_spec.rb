require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

describe ApplicationHelper do
  it "should set a proper sign" do
    helper.s(10).should == "+10"
    helper.s(-10).should == "-10"
    helper.s(0).should == "0"
  end
end
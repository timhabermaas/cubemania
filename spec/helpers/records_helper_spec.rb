require "spec_helper"

describe RecordsHelper do
  describe "#record_format" do
    it "returns 'Single Record' if amount is 1" do
      helper.record_format(stub(:amount => 1)).should == "Single Record"
    end

    it "returns 'Average of 5 Record' if amount is 5" do
      helper.record_format(stub(:amount => 5)).should == "Average of 5 Record"
    end

    it "returns 'Average of 12 Record' if amount is 12" do
      helper.record_format(stub(:amount => 12)).should == "Average of 12 Record"
    end
  end
end

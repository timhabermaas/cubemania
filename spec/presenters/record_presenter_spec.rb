require "app/presenters/record_presenter"

describe RecordPresenter do
  let(:record_presenter)

  describe "#record_format" do
    it "returns 'Single Record' if amount is 1" do
      r = RecordPresenter.new(stub(:amount => 1))
      r.record_format.should == "Single Record"
    end

    it "returns 'Average of 5 Record' if amount is 5" do
      r = RecordPresenter.new(stub(:amount => 5))
      r.record_format.should == "Average of 5 Record"
    end

    it "returns 'Average of 12 Record' if amount is 12" do
      r = RecordPresenter.new(stub(:amount => 12))
      r.record_format.should == "Average of 12 Record"
    end
  end
end

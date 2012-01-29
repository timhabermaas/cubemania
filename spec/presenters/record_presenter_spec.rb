require "app/presenters/record_presenter"

describe RecordPresenter do
  let(:record_presenter)

  describe "#record_type" do
    it "returns 'Single' if amount is 1" do
      r = RecordPresenter.new(stub(:amount => 1))
      r.record_type.should == "Single"
    end

    it "returns 'Average of 5' if amount is 5" do
      r = RecordPresenter.new(stub(:amount => 5))
      r.record_type.should == "Average of 5"
    end

    it "returns 'Average of 12' if amount is 12" do
      r = RecordPresenter.new(stub(:amount => 12))
      r.record_type.should == "Average of 12"
    end
  end

  describe "#flash_message" do
    it "returns 'You have a new average of 12 record: 32.12s'" do
      r = RecordPresenter.new(stub(:amount => 12, :human_time => "32.12s"))
      r.flash_message.should == 'You have a new average of 12 record: <strong>32.12s</strong>!'
    end
  end
end

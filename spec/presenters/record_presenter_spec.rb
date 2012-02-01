require "spec_helper"

describe RecordPresenter do
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

  describe "#singles_as_text" do
    let(:single_1) { create :single, :time => 12320 }
    let(:single_2) { create :dnf_single, :time => 9000 }
    let(:single_3) { create :single, :time => 12440 }
    let(:single_4) { create :single, :time => 20000 }
    let(:single_5) { create :single, :time => 13370 }
    let(:single_with_penalty) { create :plus2_single, :time => 14210 }
    let(:record) { create :record, :singles => [single_1, single_2, single_3, single_4, single_5], :amount => 5 }
    let(:record_2) { create :record, :singles => [single_1, single_2, single_3, single_4, single_with_penalty], :amount => 5 }
    let(:single_record) { create :record, :singles => [single_1], :amount => 1 }

    it "returns '(12.32) (DNF) 12.44 20.00 13.37'" do
      text = RecordPresenter.new(record).singles_as_text
      text.should == "(12.32) (DNF) 12.44 20.00 13.37"
    end

    it "adds a '+' to singles with penalty" do
      text = RecordPresenter.new(record_2).singles_as_text
      text.should == "(12.32) (DNF) 12.44 20.00 14.21+"
    end

    it "doesn't put parentheses around single records" do
      text = RecordPresenter.new(single_record).singles_as_text
      text.should == "12.32"
    end
  end
end

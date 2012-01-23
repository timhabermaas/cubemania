require 'spec_helper'

describe Average do
  describe "validations" do
    it { should validate_presence_of :time }
    it { should validate_presence_of :user_id }
    it { should validate_presence_of :puzzle_id }
    it { should validate_presence_of :competition_id }
  end

  it "sets time to average" do
    cubing_average = stub(:time => 1337)
    singles = FactoryGirl.build_list :single, 5
    CubingAverage.should_receive(:new).with(singles).and_return(cubing_average)
    average = FactoryGirl.build :average, :time => nil, :singles => singles
    average.save!
    average.time.should == 1337
  end
end

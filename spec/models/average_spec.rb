require 'spec_helper'

describe Average do
  describe "validations" do
    it { should validate_presence_of :time }
    it { should validate_presence_of :user_id }
    it { should validate_presence_of :puzzle_id }
    it { should validate_presence_of :competition_id }
  end

  it "calculates average before validation" do
    cubing_average = stub(:time => 1337, :dnf? => false)
    singles = FactoryGirl.build_list :single, 5
    CubingAverage.should_receive(:new).with(singles).and_return(cubing_average)
    average = FactoryGirl.build :average, :time => nil, :singles => singles
    average.valid?
    average.time.should == 1337
  end

  it "sets average to dnf if the " do
    cubing_average = stub(:time => nil, :dnf? => true)
    singles = FactoryGirl.build_list :single, 5
    CubingAverage.should_receive(:new).with(singles).and_return(cubing_average)
    average = Average.new(:singles => singles)
    average.valid?
    average.dnf.should == true
  end
end

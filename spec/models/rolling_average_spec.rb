require 'spec_helper'

describe RollingAverage do
  it "should return nil for an invalid average" do
    average = RollingAverage.new(5)
    average << Single.new(:time => 2)
    average << Single.new(:time => 4)
    average << Single.new(:time => 1)
    average << Single.new(:time => 3)
    average << Single.new(:time => 4)
    average.average.should == 2
    average << Single.new(:time => 4, :dnf => true)
    average.average.should == 3.6
  end
end

require 'spec_helper'

describe RollingAverage do
  it "should return nil for an invalid average" do
    average = RollingAverage.new(5)
    average << Single.new(:time => 2)
    average << Single.new(:time => 4)
    average << Single.new(:time => 1)
    average << Single.new(:time => 3)
    average << Single.new(:time => 4)
    average.average.should == 3
    average << Single.new(:time => 0)
    ("%.2f" % average.average).should == "2.67"
    average << Single.new(:time => 2, :dnf => true)
    ("%.2f" % average.average).should == "2.67"
    average << Single.new(:time => 10, :dnf => true)
    average.average.should be_nil
    average << Single.new(:time => 4)
    average << Single.new(:time => 3)
    average << Single.new(:time => 2)
    average << Single.new(:time => 5)
    average.average.should == 4
    average << Single.new(:time => 2)
    average.average.should == 3
  end

  it "shouldn't crash if all times are DNFs" do
    average = RollingAverage.new(3)
    average << Single.new(:time => 2, :dnf => true)
    average << Single.new(:time => 4, :dnf => true)
    average << Single.new(:time => 1, :dnf => true)

    lambda {
      average.average
    }.should_not raise_error
  end

  it "should return nil if average is requested before x times are added" do
    average = RollingAverage.new(3)
    average << Single.new(:time => 2)
    average.average.should be_nil
    average << Single.new(:time => 2)
    average.average.should be_nil
  end

  it "should return nil if no times are added" do
    RollingAverage.new(5).average.should be_nil
  end

  it "should work for average of 1" do
    average = RollingAverage.new(1)
    average << Single.new(:time => 5, :dnf => false)
    average.average.should == 5
    average << Single.new(:time => 3, :dnf => true)
    average.average.should be_nil
    average << Single.new(:time => 3)
    average.average.should == 3
  end
end

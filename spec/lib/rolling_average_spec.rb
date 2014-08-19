require "spec_helper"

describe RollingAverage do
  it "should return nil for an invalid average" do
    average = RollingAverage.new(5)
    average << Single.new(:time => 2)
    average << Single.new(:time => 4)
    average << Single.new(:time => 1)
    average << Single.new(:time => 3)
    average << Single.new(:time => 4)
    average.time.should == 3
    average << Single.new(:time => 0)
    ("%.2f" % average.time).should == "2.67"
    average << Single.new(:time => 2, :penalty => "dnf")
    ("%.2f" % average.time).should == "2.67"
    average << Single.new(:time => 10, :penalty => "dnf")
    average.time.should be_nil
    average << Single.new(:time => 4)
    average << Single.new(:time => 3)
    average << Single.new(:time => 2)
    average << Single.new(:time => 5)
    average.time.should == 4
    average << Single.new(:time => 2)
    average.time.should == 3
  end

  it "shouldn't crash if all times are DNFs" do
    average = RollingAverage.new(3)
    average << Single.new(:time => 2, :penalty => "dnf")
    average << Single.new(:time => 4, :penalty => "dnf")
    average << Single.new(:time => 1, :penalty => "dnf")

    average.time.should == nil
  end

  it "should return nil if average is requested before x times are added" do
    average = RollingAverage.new(3)
    average << Single.new(:time => 2)
    average.time.should be_nil
    average << Single.new(:time => 2)
    average.time.should be_nil
  end

  it "should return nil if no times are added" do
    RollingAverage.new(5).time.should be_nil
  end

  it "should work for average of 1" do
    average = RollingAverage.new(1)
    average << Single.new(:time => 5)
    average.time.should == 5
    average << Single.new(:time => 3, :penalty => "dnf")
    average.time.should be_nil
    average << Single.new(:time => 3)
    average.time.should == 3
  end

  describe "#singles" do
    it "should return an instance of Array" do
      RollingAverage.new(5).singles.should be_instance_of Array
    end
  end

end

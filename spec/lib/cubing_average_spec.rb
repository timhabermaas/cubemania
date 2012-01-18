require "cubing_average"

describe CubingAverage do
  it "saves singles" do
    singles = [stub, stub]
    CubingAverage.new(singles).singles.should == singles
  end

  it "defaults to an empty single array" do
    CubingAverage.new.singles.should == []
  end

  it "allows more averages to be pushed in it" do
    old_single = stub
    new_single = stub
    average = CubingAverage.new([old_single])
    average << new_single
    average.singles.should == [old_single, new_single]
  end

  it "doesn't mess with the original singles array" do
    single = stub
    singles = [single]
    average = CubingAverage.new(singles)
    singles << stub
    average.singles.should == [single]
  end

  context "#time" do
    let(:single_5) { stub :time => 5, :dnf? => false }
    let(:single_6) { stub :time => 6, :dnf? => false }
    let(:single_7) { stub :time => 7, :dnf? => false }
    let(:single_10) { stub :time => 10, :dnf? => false }
    let(:single_dnf) { stub :time => 20, :dnf? => true }

    context "one single" do
      it "returns the time" do
        CubingAverage.new([single_5]).time.should == 5
      end

      it "returns nil if the single is DNF" do
        CubingAverage.new([single_dnf]).time.should == nil
      end
    end

    context "five singles" do
      it "returns 10 for [10, 6, 7, 5, 5]" do
        singles = [single_10, single_6, single_7, single_5, single_5]
        CubingAverage.new(singles).time.should == 6
      end

      it "returns 5 for [dnf, 7, 6, 5, 5]" do
        singles = [single_dnf, single_7, single_6, single_5, single_5]
        CubingAverage.new(singles).time.should == 6
      end

      it "returns nil for [dnf, 5, 5, 5, dnf]" do
        singles = [single_dnf] + [single_5] * 3 + [single_dnf]
        CubingAverage.new(singles).time.should == nil
      end
    end
  end

  context "#dnf?" do
    it "returns true if time is nil" do
      average = CubingAverage.new
      average.stub(:time => nil)
      average.should be_dnf
    end
  end
end

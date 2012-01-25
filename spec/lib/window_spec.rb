require "window"

describe Window do
  let(:window) { Window.new([1, 2, 3, 4], 4) }

  it "accepts an array and a frame size" do
    window.array.should == [1, 2, 3, 4]
    window.frame_size == 4
  end

  describe "#<<" do
    it "pushes the last element out" do
      window << 5
      window.array.should == [2, 3, 4, 5]
    end

    it "doesn't push anything out if there are too few elements" do
      w = Window.new [1, 2], 3
      w << 6
      w.array.should == [1, 2, 6]
    end
  end

  describe "#size" do
    it "returns the size of the array" do
      Window.new([1, 2], 5).size.should == 2
    end
  end

  describe "#empty?" do
    it "returns true for empty array, false for non-empty arrays" do
      w = Window.new([], 1)
      w.should be_empty
      w = Window.new([2], 1)
      w.should_not be_empty
    end
  end
end


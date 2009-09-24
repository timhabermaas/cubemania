require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

describe Average do
  
  it "should create a new instance given valid attributes" do
    average = Factory.build(:average)
    average.should be_valid
  end
  
  it "should give an error if there are not enough singles for an average" do
    average = Factory.build(:average)
    average.singles.delete(average.singles.first)
    average.should_not be_valid
  end
  
end

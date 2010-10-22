require 'spec_helper'

describe Average do
  it "should return nil for an invalid average" do
    a = Average.new([Factory.build(:dnf_single), Factory.build(:dnf_single), Factory.build(:dnf_single), Factory.build(:single), Factory.build(:single)])
    a.time.should be_nil
  end

  it "should give a correct average" do
    a = Average.new([Factory.build(:single, :time => 1000),
                 Factory.build(:dnf_single),
                 Factory.build(:single, :time => 2000),
                 Factory.build(:single, :time => 4000),
                 Factory.build(:single, :time => 6000)])
    a.time.to_i.should == 4000
  end
end

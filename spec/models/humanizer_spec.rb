require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

class Dummy
  extend Humanizer
  attr_accessor :blub

  humanize :blub => :time
end

describe Humanizer do
  subject { Dummy.new }

  it { should respond_to(:human_blub) }

  describe "#human_attribute" do
    let(:dummy) { Dummy.new }

    context "when not passed a spacer" do
      subject { dummy.human_blub }

      it "should return 10.21s for 10210" do
        dummy.blub = 10210
        should == "10.21s"
      end

      it "should return 10.22s for 10215" do
        dummy.blub = 10215
        should == "10.22s"
      end

      it "should return 12:10.32min for 730320" do
        dummy.blub = 730320
        should == "12:10.32min"
      end
    end

    context "when passed a spacer" do
      subject { dummy.human_blub('_#') }

      it "should return 10.21_#s for 10210" do
        dummy.blub = 10210
        should == "10.21_#s"
      end
    end
  end
end
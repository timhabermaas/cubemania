require "spec_helper"

describe Activity do
  describe "#corrupt?" do
    context "trackable doesn't exist" do
      before do
        subject.stub(:trackable => nil)
      end
      its(:corrupt?) { should == true}
    end

    context "trackable exists" do
      before do
        subject.stub(:trackable => stub)
      end

      its(:corrupt?) { should == false}
    end
  end
end

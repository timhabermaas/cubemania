require "mean"

describe Mean do
  let(:single_1) { stub(:single, :dnf? => false, :time => 42) }
  let(:single_2) { stub(:single, :dnf? => false, :time => 41) }
  let(:singles) { [single_1, single_2] }

  context "zero DNFs" do
    subject { Mean.new(singles) }

    its(:result) { should == 41.5 }
  end

  context "one DNF" do
    let(:single_dnf) { stub(:single, :dnf? => true) }

    subject { Mean.new(singles << single_dnf) }

    its(:result) { should == nil }
  end
end

require "average"
require "comparable_solve"

describe Average do
  let(:single_1) { stub :single, :dnf? => false, :time => 40 }
  let(:single_2) { stub :single, :dnf? => false, :time => 41 }
  let(:single_3) { stub :single, :dnf? => false, :time => 42 }
  let(:single_4) { stub :single, :dnf? => false, :time => 61 }
  let(:single_dnf) { stub :single, :dnf? => true, :time => 20 }
  let(:singles) { [single_1, single_2, single_3, single_4] }

  before do
    singles.each do |s|
      s.extend ComparableSolve
    end
    single_dnf.extend ComparableSolve
  end

  context "zero DNFs" do
    subject { Average.new singles }

    its(:result) { should == 41.5 }
  end

  context "one DNF" do
    subject { Average.new singles << single_dnf }

    its(:result) { should == 48 }
  end

  context "more than one DNF" do
    subject { Average.new singles << single_dnf << single_dnf }

    its(:result) { should == nil }
  end
end

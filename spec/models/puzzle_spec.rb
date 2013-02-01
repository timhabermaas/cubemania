require "spec_helper"

describe Puzzle do
  describe "#full_name" do
    let(:subject) { Puzzle.new(:name => "3x3x3") }

    before do
      subject.stub(:kind => kind)
    end

    context "no kind name available" do
      let(:kind) { stub(:kind, :short_name => "") }

      it "returns only '3x3x3'" do
        expect(subject.full_name).to eq("3x3x3")
      end
    end

    context "kind name available" do
      let(:kind) { stub(:kind, :short_name => "BLD") }

      it "returns '3x3x3 BLD'" do
        expect(subject.full_name).to eq("3x3x3 BLD")
      end
    end
  end
end

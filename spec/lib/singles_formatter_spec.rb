require "spec_helper" # TODO remove dependency by moving the concept of a time into extra class. FUCK YOU AcitveRecord!

describe "SinglesFormatter" do
  describe "#as_text" do
    let(:subject) { SinglesFormatter.new singles }

    context "one single" do
      let(:singles) { [Single.new(:time => 12320)] }

      it "returns 12.32 for one single with that time" do
        expect(subject.as_text).to eq("12.32")
      end
    end

    context "more singles" do
      let(:single_1) { Single.new :time => 3210 }
      let(:single_2) { Single.new :time => 10210 }
      let(:single_3) { Single.new :time => 7210 }
      let(:singles) { [single_1, single_2, single_3] }

      it "puts best and worst time in parantheses" do
        expect(subject.as_text).to eq("(3.21) (10.21) 7.21")
      end
    end

    context "one DNF" do
      let(:single_1) { Single.new :time => 21320 }
      let(:single_2) { Single.new :time => 10000, :penalty => "dnf" } # TODO internals
      let(:single_3) { Single.new :time => 12000 } # TODO internals

      let(:singles) { [single_1, single_2, single_3] }

      it "prints DNF instead of time" do
        expect(subject.as_text).to eq("21.32 (DNF) (12.00)")
      end
    end

    context "+2" do
      let(:singles) { [Single.new(:time => 12000, :penalty => "plus2")] } # TODO internals

      it "displays a '+' after time" do
        expect(subject.as_text).to eq("12.00+")
      end
    end
  end
end

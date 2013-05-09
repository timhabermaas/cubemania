require_relative "../../lib/recalculate_records_history"
require_relative "../../lib/mean"

describe RecalculateRecordsHistory do
  subject { RecalculateRecordsHistory }
  let(:record_class) {
    Class.new do
      attr_accessor :time
      def initialize(args)
        self.time = args[:time]
      end
    end
  }
  let(:type) { stub(:type, :count => 5, :calculator => Mean) }

  def stub_singles(*times)
    times.map do |t|
      stub :single, :time => t, :dnf? => t.nil?
    end
  end

  context "too less singles for a record" do
    let(:singles) { [stub] * 4 }
    let(:old_record) { stub }

    it "returns []" do
      expect(subject.for(type, singles, old_record, record_class)).to eq([])
    end
  end

  context "enough singles" do
    let(:singles) { stub_singles(1, 5, 5, 2, 2, 1, 0, nil, nil) }
    let(:result) { subject.for(type, singles, old_record, record_class) }

    context "no old_record" do
      let(:old_record) { nil }

      it "returns all new records" do
        expect(result).to have(2).elements
        expect(result[0].time).to eq(3)
        expect(result[1].time).to eq(2)
      end
    end

    context "old record present" do
      let(:old_record) { stub(:record, :time => 3) }

      it "returns just the last record" do
        expect(result).to have(1).elements
        expect(result[0].time).to eq(2)
      end
    end
  end
end

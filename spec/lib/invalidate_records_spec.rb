require_relative "../../lib/invalidate_records"

describe InvalidateRecords do
  describe ".for" do
    let(:single) { stub(:single, :created_at => DateTime.new(2013, 2, 4)) }
    let(:record_class) { stub(:record_class) }
    let(:younger_records) { [stub, stub] }

    it "destroys all records which are younger than a given single" do
      record_class.should_receive(:younger_than).
                   with(single).
                   and_return(younger_records)
      younger_records.each do |r|
        r.should_receive(:destroy)
      end
      InvalidateRecords.for(single, record_class)
    end
  end
end

require_relative "../../app/models/facebook_post"

describe FacebookPost do
  let(:singles_formatter) { stub(:formatter, :as_text => "(12.32) 14.32 (DNF)") }
  let(:record) { stub(:record) }
  let(:subject) { FacebookPost.new(record, singles_formatter) }

  describe "#body" do
    context "single record" do
      before do
        record.stub_chain(:type, :single?).and_return(true)
        record.stub_chain(:singles, :first, :scramble).and_return("R2 D2")
      end

      it "returns the times" do
        expect(subject.body).to eq("R2 D2")
      end
    end

    context "average record" do
      before do
        record.stub_chain(:type, :single?).and_return(false)
      end

      it "returns the times" do
        expect(subject.body).to eq("(12.32) 14.32 (DNF)")
      end
    end
  end

  describe "#caption" do
    it "returns a neat slogan" do
      expect(subject.caption).to eq("Keep track of your times and join Cubemania!")
    end
  end

  describe "#title" do # TODO stub awkwardness
    let(:user) { stub(:user, :name => "peter") }
    let(:puzzle) { stub(:puzzle, :full_name => "5x5x5 BLD") }
    let(:record_type) { stub(:record_type, :full_name => "average of 5") }
    let(:record) { stub(:record, :human_time => "2:42.12min",
                                 :user => user,
                                 :puzzle => puzzle,
                                 :type => record_type) }

    it "mentions user name, puzzle, record type and time" do
      expect(subject.title).to eq("Peter has a new 5x5x5 BLD average of 5 record: 2:42.12min")
    end
  end
end
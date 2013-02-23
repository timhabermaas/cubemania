require "spec_helper"

describe CubingSession do
  describe "singles" do
    let(:user) { create :user }
    let(:puzzle) { create :puzzle }
    let(:singles) { create_list :single, 2, :user => user, :puzzle => puzzle }

    it "has no singles per default" do
      expect(CubingSession.new.singles).to eq []
    end

    it "can save and retrieve singles" do
      CubingSession.create :user => user, :puzzle => puzzle, :singles => singles
      expect(CubingSession.first.singles).to eq(singles)
    end

    it "can add singles" do
      session = CubingSession.new
      single = create :single
      session.add_single(single)
      expect(session.singles).to eq [single]
    end

    xit "checks if puzzle matches singles"
  end
end

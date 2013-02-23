require "spec_helper"

describe CubingSession do
  let(:user) { create :user }
  let(:puzzle) { create :puzzle }

  describe "singles" do
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

  describe "#too_old?" do
    let(:single_1) { create :single, :created_at => 2.days.ago }
    let(:single_2) { create :single, :created_at => 5.hours.ago }
    let(:singles) { [single_1, single_2] }
    let(:single) { create :single, :created_at => Time.now }

    subject { CubingSession.new(:singles => singles) }

    context "stopping of last single is past provided time span" do
      it "is too old" do
        expect(subject.too_old?(single, 2.hours)).to eq true
      end
    end

    context "stopping of last single is still in the time span" do
      it "is not too old" do
        expect(subject.too_old?(single, 6.hours)).to eq false
      end
    end
  end

  describe "#new?" do
    context "no single" do
      subject { CubingSession.new :singles => [] }
      it { should be_new }
    end

    context "one single" do
      subject { CubingSession.new :singles => [create(:single)] }
      it { should be_new }
    end

    context "more than one single" do
      subject { CubingSession.new :singles => create_list(:single, 2) }
      it { should_not be_new }
    end
  end

  describe ".create_from_single" do
    let(:single) { create :single }

    subject { CubingSession.create_from_single(single) }

    its(:singles) { should == [single] }
    its(:user) { should == single.user }
    its(:puzzle) { should == single.puzzle }
  end

  describe "scopes" do
    let!(:session_1) { create :cubing_session, :user => user, :puzzle => puzzle, :updated_at => 1.month.ago }
    let!(:session_2) { create :cubing_session, :user => user, :puzzle => puzzle, :updated_at => 1.day.ago }
    let!(:session_3) { create :cubing_session, :user => user, :puzzle => puzzle, :updated_at => 2.days.ago }
    let!(:session_4) { create :cubing_session, :user => create(:user), :puzzle => puzzle, :updated_at => 1.hour.ago }

    describe ".for" do
      it "grabs all sessions for that particular user and puzzle" do
        expect(CubingSession.for(user.id, puzzle.id)).to eq([session_1, session_2, session_3])
      end
    end

    describe ".last_for" do
      it "grabs the latest session for this user and puzzle" do
        expect(CubingSession.last_for(user.id, puzzle.id)).to eq(session_2)
      end
    end
  end
end

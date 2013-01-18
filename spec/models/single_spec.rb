require "spec_helper"

describe Single do
  let(:single) { build(:single, :user => user, :puzzle => puzzle, :time => 20000) }
  let(:user) { create(:user) }
  let(:puzzle) { create(:puzzle) }

  subject { single }

  describe "validations" do
    it { should be_valid }
    it { should validate_presence_of(:time) }
    it { should validate_presence_of(:user_id) }
    it { should validate_presence_of(:puzzle_id) }
    it { should allow_value("dnf").for(:penalty) }
    it { should allow_value("plus2").for(:penalty) }
    it { should_not allow_value("m").for(:penalty) }
  end

  it { should_not allow_mass_assignment_of(:user_id) }

  it "sets penalty to nil before validation if it's left blank" do
    single = Single.new :penalty => " "
    single.valid?
    single.penalty.should == nil
  end

  describe "scopes" do
    describe ".grouped" do # TODO add options for sum/avg/count
      let(:october_4) { Time.new(2012, 10, 4) }
      let(:october_5) { Time.new(2012, 10, 5) }
      let(:october_20) { Time.new(2012, 10, 20) }
      let(:november_2) { Time.new(2012, 11, 2) }
      let(:december_3) { Time.new(2012, 12, 3) }

      before :each do
        create :single, :time => 1000, :created_at => october_4, :comment => "Best solve ever!"
        create :single, :time => 4000, :created_at => october_5, :comment => "Even better!"
        create :single, :time => 4000, :created_at => october_20
        create :single, :time => 3000, :created_at => october_20
        create :single, :time => 5000, :created_at => november_2
      end

      context "by: day" do
        subject { Single.grouped(by: :day).sort_by { |s| s.created_at } }

        it "merges singles which were done on the same day" do
          expect(subject).to have(4).items
          expect(subject[2].time).to eq(3500)
        end
      end

      context "by: week" do
        subject { Single.grouped(by: :week).sort_by { |s| s.created_at } }

        it "merges singles which were done in the same week" do
          expect(subject).to have(3).items
        end
      end

      context "by: month" do
        subject { Single.grouped(by: :month).sort_by { |s| s.created_at } }

        it "merges singles which were done in the same month" do
          expect(subject).to have(2).items
          expect(subject[0].time).to eq(3000)
          expect(subject[1].time).to eq(5000)
        end
      end

      context "timezone: Berlin" do
        let(:time_zone) { ActiveSupport::TimeZone.new("Berlin") }

        it "is timezone aware" do
          Single.delete_all # TODO
          create :single, :time => 10, :created_at => time_zone.local(2012, 1, 1, 20, 00)
          create :single, :time => 2, :created_at => time_zone.local(2012, 1, 1, 23, 30)
          create :single, :time => 100, :created_at => time_zone.local(2012, 1, 2, 0, 30)
          create :single, :time => 20, :created_at => time_zone.local(2012, 1, 2, 4, 00)
          grouped = Single.grouped(by: :day, time_zone: "Berlin").order("created_at")
          expect(grouped.first.time).to eq(6)
          expect(grouped.last.time).to eq(60)
        end
      end

      describe "comments" do
        it "concatenates comments" do
          singles = Single.grouped(by: :month).sort_by { |s| s.created_at }
          expect(singles[0].comment).to eq("Best solve ever!\nEven better!")
        end
      end

      context "by: invalid_interval" do
        it "raises ArgumentError" do
          expect { Single.grouped(by: :invalid_interval) }.to raise_error(ArgumentError)
        end
      end
    end
  end

  describe "updating comments" do
    let(:record) { double :record }

    it "triggers comment updating when changing comment" do
      single = create :single

      single.stub(:records) { [record, record] }
      record.should_receive(:update_comment!).twice
      single.update_attributes(:comment => "OMG! Awesome solve!")
    end

    it "doesn't trigger comment updating when comment didn't change" do
      single = create :single
      single.stub(:records) { [record] }
      record.should_not_receive(:update_comment!)
      single.update_attributes(:penalty => "dnf")
    end
  end

  describe "#toggle_dnf" do
    subject { single.dnf? }

    context "when no penalty" do
      before { single.penalty = nil }
      it "should become true" do
        single.toggle_dnf!
        should == true
      end
    end

    context "when plus2" do
      before { single.penalty = "plus2" }
      it "should become true and decrease time" do
        single.toggle_dnf!
        should == true
        single.time.should == 18000
      end
    end

    context "when dnf" do
      before { single.penalty = "dnf" }
      it "should become false" do
        single.toggle_dnf!
        should == false
      end
    end
  end

  describe "#toggle_plus2" do
    subject { single.plus2? }

    context "when no penalty" do
      before { single.penalty = nil }
      it "should increase time and update flag" do
        single.toggle_plus2!
        should == true
        single.time.should == 22000
      end
    end

    context "when dnf" do
      before { single.penalty = "dnf" }
      it "should increase time and update flag" do
        single.toggle_plus2!
        should == true
        single.time.should == 22000
      end
    end

    context "when plus2" do
      before { single.penalty = "plus2" }
      it "should decrease time and update flag" do
        single.toggle_plus2!
        should == false
        single.time.should == 18000
      end
    end
  end
end

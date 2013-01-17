require "spec_helper"

describe UsersHelper do
  describe "#month_beginning_in_this_week" do
    it "returns nil if there's no month starting in this week" do
      beginning = helper.month_beginning_in_this_week(Time.new(2012, 4, 15))
      expect(beginning).to eq(nil)
    end

    it "returns January 1st, 2013 for December 31, 2012" do
      beginning = helper.month_beginning_in_this_week(Time.new(2012, 12, 31))
      expect(beginning.to_i).to eq(Time.new(2013, 1, 1).to_i)
    end

    it "returns the next month if it starts on sunday" do
      beginning = helper.month_beginning_in_this_week(Time.new(2012, 6, 25))
      expect(beginning.to_i).to eq(Time.new(2012, 7, 1).to_i)
    end

    it "returns the current month if the beginning of the week is the first" do
      beginning = helper.month_beginning_in_this_week(Time.new(2011, 8, 1))
      expect(beginning.to_i).to eq(Time.new(2011, 8, 1).to_i)
    end
  end
end

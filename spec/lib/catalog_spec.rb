require "catalog"
require "active_support/all"

describe Catalog do
  it "groups by day" do
    single_1 = stub(:created_at => DateTime.new(2010, 1, 1, 14, 0))
    single_2 = stub(:created_at => DateTime.new(2010, 2, 1, 14, 0))
    single_3 = stub(:created_at => DateTime.new(2010, 2, 1, 16, 0))
    single_4 = stub(:created_at => DateTime.new(2010, 3, 4, 18, 0))
    catalog = Catalog.by_day([single_1, single_2, single_3, single_4])
    catalog[Day.new(2010, 1, 1)].should == [single_1]
    catalog[Day.new(2010, 2, 1)].should == [single_2, single_3]
    catalog[Day.new(2010, 3, 4)].should == [single_4]
  end

  it "groups by month" do
    single_1 = stub(:created_at => DateTime.new(2010, 4, 1, 14, 0))
    single_2 = stub(:created_at => DateTime.new(2010, 4, 10, 14, 0))
    single_3 = stub(:created_at => DateTime.new(2010, 6, 1, 16, 0))
    single_4 = stub(:created_at => DateTime.new(2010, 6, 4, 18, 0))
    catalog = Catalog.by_month([single_1, single_2, single_3, single_4])
    catalog[Month.new(2010, 4)].should == [single_1, single_2]
    catalog[Month.new(2010, 6)].should == [single_3, single_4]
  end
end


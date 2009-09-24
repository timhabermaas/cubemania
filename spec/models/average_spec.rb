require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

describe Average do
  before(:each) do
    @valid_attributes = {
    }
  end

  it "should create a new instance given valid attributes" do
    User.create!(:name => 'peter', :password => 'dieter', :password_confirmation => 'dieter', :email => 'test@dieter.de', :bot_email => '')
    user = Login.new({:name => 'peter', :password => 'dieter'}).validate
    puzzle = Puzzle.create!(:name => '3x3x3', :kind_id => 2, :scramble_length => 25, :attempt_count => 5, :countdown => 0, :average_format => 'average', :image => 'test.jpg')
    average = user.averages.build :time => 12312, :puzzle => puzzle, :record => false, :dnf => false
    average.singles = (1..5).to_a.map {|i| user.singles.build :time => 12, :puzzle => puzzle, :dnf => false, :position => i}
    average.save!
  end
end

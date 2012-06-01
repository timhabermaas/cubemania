require "spec_helper"

describe Login do

  it "should authenticate with matching password and username" do
    user = create(:user, :name => 'Bob', :password => 'secret')
    Login.new(:name => 'Bob', :password => 'secret').validate.should == user
  end

  it "should not authenticate with incorrect password" do
    user = create(:user, :name => 'Bob', :password => 'secret')
    Login.new(:name => 'Bob', :password => 'seCret').validate.should be_nil
  end

end

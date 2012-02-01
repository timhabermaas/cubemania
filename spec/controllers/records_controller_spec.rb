require "spec_helper"

describe RecordsController do
  describe "#share" do
    let(:user) { create :user }
    let(:record) { create :record, :user => user, :amount => 5, :time => 12420 }

    before :each do
      controller.stub!(:current_user) { user }
    end

    context "user is already authorized" do
      before :each do
        user.authorizations << Authorization.new(:provider => "facebook", :token => "foo_token", :uid => "dasd")
      end

      it "posts on fb wall" do
        me = stub :first_name => "Rowe"
        blub = stub :fetch => me
        FbGraph::User.should_receive(:me).with("foo_token").and_return(blub)
        me.should_receive(:feed!).with(hash_including(:link => puzzle_record_url(record.puzzle, record),
                                                      :name => "Rowe has a new Average of 5 Record: 12.42s"))
        post :share, :id => record.id
      end
    end

    context "user is not authorized" do
      it "redirects to /auth/facebook" do
        FbGraph::User.should_not_receive(:me)
        post :share, :id => record.id
        response.should redirect_to("/auth/facebook")
      end
    end

    context "user is authorized, but access token is no longer valid" do
      before :each do
        user.authorizations << Authorization.new(:provider => "facebook", :token => "invalid_token", :uid => "dasd")
      end

      it "redirects to /auth/facebook" do
        post :share, :id => record.id
        response.should redirect_to("/auth/facebook")
      end
    end
  end
end
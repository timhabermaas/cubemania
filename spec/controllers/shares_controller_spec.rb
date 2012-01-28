require "spec_helper"

describe SharesController do
  describe "#create" do
    context "provider = facebook" do
      let(:user) { create :user }
      let(:record) { create :record, :user => user, :amount => 5 }

      before :each do
        user.authorizations << Authorization.new(:provider => "facebook", :token => "foo_token", :uid => "dasd")
        controller.stub!(:current_user) { user }
      end

      it "posts on fb wall" do
        me = stub
        FbGraph::User.should_receive(:me).with("foo_token").and_return(me)
        me.should_receive(:feed!).with(hash_including(:link => "http://test.host/puzzles/#{record.puzzle.slug}/records/#{record.id}", :name => "Average of 5"))
        post :create, :record_id => record.id
      end
    end
  end
end
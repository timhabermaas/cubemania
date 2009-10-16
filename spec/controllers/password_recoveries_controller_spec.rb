require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

describe PasswordRecoveriesController do

  describe "GET 'show'" do
    it "should be successful" do
      get :show
      response.should render_template(:show)
      response.should be_success
    end
  end
  
  describe "POST 'create'" do
    it "should reset the password of the given user which belongs to the email" do
      user = Factory.create(:user, :email => 'muh@blub.com')
      #user.should_receive(:reset_password!).once
      post :create, { :recovery => {:email => 'muh@blub.com'} }
      response.should redirect_to(root_path)
      flash[:notice].should match(/password has been sent/)
    end
    
    it "should render show again if the given email doesn't exist" do
      User.destroy_all
      user = Factory.create(:user, :email => 'muh@blub2.com')
      post :create, :recovery => {:email => 'muh@blub.com'}
      response.should render_template(:show)
    end
  end
end

require "spec_helper"

describe UserMailer do
  describe "reset_password" do
    let(:user) { create :user }
    let(:mail) { UserMailer.reset_password(user, "fooo123") }

    it "sends password to user" do
      mail.subject.should == "New Password for Cubemania"
      mail.to.should == [user.email]
      mail.from.should == ["info@cubemania.org"]
      mail.body.encoded.should match(user.name)
      mail.body.encoded.should match(login_url)
      mail.body.encoded.should match("fooo123")
    end
  end
end

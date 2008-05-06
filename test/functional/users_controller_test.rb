require File.dirname(__FILE__) + '/../test_helper'

class UsersControllerTest < ActionController::TestCase
  fixtures :all

  def test_should_get_index
    get :index
    assert_response :success
    assert_not_nil assigns(:users)
  end
end

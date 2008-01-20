require File.dirname(__FILE__) + '/../test_helper'

class KindsControllerTest < ActionController::TestCase
  def test_should_get_index
    get :index
    assert_response :success
    assert_not_nil assigns(:kinds)
  end

  def test_should_get_new
    get :new
    assert_response :success
  end

  def test_should_create_kind
    assert_difference('Kind.count') do
      post :create, :kind => { }
    end

    assert_redirected_to kind_path(assigns(:kind))
  end

  def test_should_show_kind
    get :show, :id => kinds(:one).id
    assert_response :success
  end

  def test_should_get_edit
    get :edit, :id => kinds(:one).id
    assert_response :success
  end

  def test_should_update_kind
    put :update, :id => kinds(:one).id, :kind => { }
    assert_redirected_to kind_path(assigns(:kind))
  end

  def test_should_destroy_kind
    assert_difference('Kind.count', -1) do
      delete :destroy, :id => kinds(:one).id
    end

    assert_redirected_to kinds_path
  end
end

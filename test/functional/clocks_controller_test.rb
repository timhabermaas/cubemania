require File.dirname(__FILE__) + '/../test_helper'

class ClocksControllerTest < ActionController::TestCase
  def test_should_get_index
    get :index
    assert_response :success
    assert_not_nil assigns(:clocks)
  end

  def test_should_get_new
    get :new
    assert_response :success
  end

  def test_should_create_clock
    assert_difference('Clock.count') do
      post :create, :clock => { }
    end

    assert_redirected_to clock_path(assigns(:clock))
  end

  def test_should_show_clock
    get :show, :id => clocks(:one).id
    assert_response :success
  end

  def test_should_get_edit
    get :edit, :id => clocks(:one).id
    assert_response :success
  end

  def test_should_update_clock
    put :update, :id => clocks(:one).id, :clock => { }
    assert_redirected_to clock_path(assigns(:clock))
  end

  def test_should_destroy_clock
    assert_difference('Clock.count', -1) do
      delete :destroy, :id => clocks(:one).id
    end

    assert_redirected_to clocks_path
  end
end

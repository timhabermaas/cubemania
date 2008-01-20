require File.dirname(__FILE__) + '/../test_helper'

class PuzzlesControllerTest < ActionController::TestCase
  def test_should_get_index
    get :index
    assert_response :success
    assert_not_nil assigns(:puzzles)
  end

  def test_should_get_new
    get :new
    assert_response :success
  end

  def test_should_create_puzzle
    assert_difference('Puzzle.count') do
      post :create, :puzzle => { }
    end

    assert_redirected_to puzzle_path(assigns(:puzzle))
  end

  def test_should_show_puzzle
    get :show, :id => puzzles(:one).id
    assert_response :success
  end

  def test_should_get_edit
    get :edit, :id => puzzles(:one).id
    assert_response :success
  end

  def test_should_update_puzzle
    put :update, :id => puzzles(:one).id, :puzzle => { }
    assert_redirected_to puzzle_path(assigns(:puzzle))
  end

  def test_should_destroy_puzzle
    assert_difference('Puzzle.count', -1) do
      delete :destroy, :id => puzzles(:one).id
    end

    assert_redirected_to puzzles_path
  end
end

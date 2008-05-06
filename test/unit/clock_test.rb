require File.dirname(__FILE__) + '/../test_helper'

class ClockTest < ActiveSupport::TestCase
  fixtures :all
  def test_truth
    assert puzzles('3x3x3').scramble_length == 25
  end
end

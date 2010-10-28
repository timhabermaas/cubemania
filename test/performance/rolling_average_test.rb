require 'test_helper'
require 'rails/performance_test_help'

class RollingAverageTest < ActionDispatch::PerformanceTest
  def setup
    @singles = []
    10000.times do
      @singles << Single.new(:time => rand(10000), :dnf => (rand(10) == 0))
    end
  end
  def test_calculating_average
    ra = RollingAverage.new(5)
    best = 10
    @singles.each do |single|
      ra << single
      ra.average
    end
  end
end

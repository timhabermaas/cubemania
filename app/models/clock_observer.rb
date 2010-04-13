class ClockObserver < ActiveRecord::Observer
  def after_create(clock)
    if clock.record?
      clock.puzzle.increment!(:version)
    end
  end
end
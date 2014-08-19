require "window"

class RollingAverage < CubingAverage
  def initialize(*arguments)
    if arguments.size == 1
      super Window.new([], arguments.first)
    elsif arguments.size == 2
      super Window.new(arguments.first, arguments.last)
    else
      raise ArgumentError, "pass in either the window size or an initial array and the window size"
    end
  end

  def time
    return nil if @singles.size < @singles.frame_size # TODO this is kinda ugly, delegate instead?
    super
  end

  def singles
    super.array
  end
end

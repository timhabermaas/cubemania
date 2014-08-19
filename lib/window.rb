class Window
  include Enumerable

  attr_accessor :array, :frame_size

  def initialize(array, frame_size)
    @array = array.dup
    @frame_size = frame_size
  end

  def <<(item)
    @array << item
    if @array.size > frame_size
      @array.shift
    end
  end

  def size
    @array.size
  end

  def empty?
    @array.empty?
  end

  def each(&block)
    @array.each(&block)
  end
end

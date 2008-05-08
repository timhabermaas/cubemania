class Puzzle < ActiveRecord::Base
  belongs_to :kind, :order => 'name'
  has_many :records, :conditions => ['record = ?', true], :order => 'time', :class_name => 'Clock' do
    def single; @single ||= find_all_by_type 'Single', :include => :user; end
    def average; @average ||= find_all_by_type 'Average', :include => :user; end
  end
  has_many :clocks, :dependent => :delete_all
  
  file_column :image, :store_dir => 'public/images/puzzles', :base_url => 'images/puzzles'
  
  def self.formats
    %w{average mean best_of}
  end
  
  validates_presence_of :name, :image, :attempt_count, :countdown, :kind_id
  validates_length_of :name, :maximum => 64
  validates_numericality_of :scramble_length, :greater_than_or_equal_to => 0, :less_than_or_equal_to => 255, :only_integer => true
  validates_numericality_of :countdown, :greater_than_or_equal_to => 0, :only_integer => true
  validates_numericality_of :attempt_count, :greater_than => 0, :only_integer => true
  validates_inclusion_of :average_format, :in => formats
  validates_filesize_of :image, :in => 0..20.kilobytes
  validates_file_format_of :image, :in => ['gif', 'png']
  
  def scramble
    case name
      when '2x2x2', '3x3x3'
        cube_scramble [%w{R L}, %w{F B}, %w{D U}]
      when '4x4x4', '5x5x5'
        cube_scramble [%w{R L r l}, %w{F B f b}, %w{D U d u}]
      when 'megaminx'
        megaminx_scramble
      when 'pyraminx'
        pyraminx_scramble
      when 'square one'
      	square1_scramble
      when 'clock'
      	clock_scramble
    end
  end
  
  private
    def cube_scramble(turns)
      variants = ['', "'", '2']
      axis = rand turns.size
      (1..scramble_length).map do
        axis = (axis + rand(turns.size - 1) + 1) % turns.size
        turns[axis].rand + variants.rand
      end.join(" ")
    end
    
    def megaminx_scramble
      scramble = ''
      turns = %w(R D)
      variants = %w(-- ++)
      scramble_length.times do |index|
        scramble += (scramble.empty? ? '' : ' ') + turns[index % 2] + variants.rand
        scramble += ' Y' + variants.rand + "<br/>" if index % 10 == 9
      end
      scramble
    end
    
    def pyraminx_scramble
      turns = %w(U L R B)
      variants = ['', "'"]
      tip_turns = turns.map &:downcase
      tip_length = rand(3) + 1
      scramble = (0..tip_length).map do
        tip_turns.delete(tip_turns.rand) + variants.rand
      end
      axis = rand turns.size
      scramble += (tip_length..scramble_length).map do
        axis = (axis + rand(turns.size - 1) + 1) % turns.size
        turns[axis] + variants.rand
      end
      scramble.join(' ')
    end
    
    def square1_scramble
      scramble = []
      degrees = {:corner => 60, :edge => 30}
    	up = (0..7).map{|i| i%2 == 0 ? 30 : 60}
    	down = []
      down.replace up
      scramble_length.times do
        up_move = check_moves(up).rand
        down_move = check_moves(down).rand
        up_move = 0 if up_move.nil?
        down_move = 0 if down_move.nil? # TODO avoid (0,0)
        scramble << [up_move, down_move * -1] # TODO return moves in degrees*30 and not steps
        do_move up, up_move
        do_move down, down_move
        do_slice(up, down)
      end
      scramble.map {|s| "(#{s.join(',')})"}.join(" / ")
    end
    
    def check_moves(layer)
      layer_moves = []
      layer.length.times do |start|
        sum = 0
        possible = false
        layer.length.times do |i|
          sum += layer[(start + i) % layer.length]
          possible = true if sum == 180
        end
        if start >= (layer.length / 2)
          start -= layer.length
        end
        layer_moves << start if possible
      end
      layer_moves
    end
    
    def do_move(layer, l)
      a = []
      l %= layer.length
      if l < 0
        l *= -1
        l.times do
          a << layer.pop
        end
        a.reverse!
        return layer.replace(a + layer)
      else
        l.times do
          a << layer.shift
        end
        return layer.replace(layer + a)
      end
    end
    
    def do_slice(up, down)
      sum = 0
      to_down = up.select {|u| sum += u; sum <= 180}
      to_down.reverse!
      sum = 0
      up.delete_if {|u| sum += u; sum <= 180}
      
      sum = 0
      to_up = down.select {|d| sum += d; sum <= 180}
      to_up.reverse!
      sum = 0
      down.delete_if {|d| sum += d; sum <= 180}
      up.insert(0, to_up).flatten!
      down.insert(0, to_down).flatten!
    end
    
    def clock_scramble
    	pins = %w(U d)
    	states = %w(UUdd dUdU ddUU UdUd dUUU UdUU UUUd UUdU UUUU dddd)
    	scramble = states.map do |state|
    	  moves = []
    		moves << 'u = ' + (rand(13) - 6).to_s if state.gsub('d', '').length > 1
    		moves << 'd = ' + (rand(13) - 6).to_s if state.gsub('U', '').length > 1
    		state + ' ' + moves.join("; ")
    	end
    	scramble << Array.new(4).map do
    		pins.rand
    	end.join
    	scramble.join(" / ")
    end
end
class Puzzle < ActiveRecord::Base
  FORMATS = %w{average mean best_of}

  belongs_to :kind
  has_many :competitions, :dependent => :destroy
  has_many :records, :conditions => ['record = ? and users.ignored = ?', true, false], :include => :user, :order => 'time', :class_name => 'Clock' do
    def single(page); @single ||= paginate_by_type 'Single', :page => page, :per_page => 50; end
    def average(page); @average ||= paginate_by_type 'Average', :page => page, :per_page => 50; end
  end
  has_many :clocks, :dependent => :delete_all

  has_attached_file :image, :path => ":rails_root/public/images/:class/:id/:style/:basename.:extension",
                            :url => "/images/:class/:id/:style/:basename.:extension",
                            :styles => { :small => "50x25" }
  attr_protected :image_file_name, :image_content_type, :image_size
  
  validates_presence_of :name, :image, :attempt_count, :countdown, :kind_id
  validates_length_of :name, :maximum => 64
  validates_numericality_of :scramble_length, :greater_than_or_equal_to => 0, :less_than_or_equal_to => 255, :only_integer => true
  validates_numericality_of :countdown, :greater_than_or_equal_to => 0, :only_integer => true
  validates_numericality_of :attempt_count, :greater_than => 0, :only_integer => true
  validates_inclusion_of :average_format, :in => FORMATS
  validates_attachment_size :image, :less_than => 20.kilobytes
  validates_attachment_content_type :image, :content_type => ['image/png', 'image/gif']
  
  def scrambles
    (1..attempt_count).map {|i| scramble}
  end
  
  def scramble
    case name.downcase
      when '2x2x2', '3x3x3'
        cube_scramble [%w{R L}, %w{F B}, %w{D U}]
      when '4x4x4', '5x5x5'
        cube_scramble [%w{R L Rw Lw}, %w{F B Fw Bw}, %w{D U Dw Uw}]
      when '6x6x6', '7x7x7'
        cube_scramble [['R', 'L', 'R&sup2;', 'L&sup2;', 'R&sup3;', 'L&sup3;'], ['F', 'B', 'F&sup2;', 'B&sup2;', 'F&sup3;', 'B&sup3;'], ['D', 'U', 'D&sup2;', 'U&sup2;', 'D&sup3;', 'U&sup3;']]
      when 'megaminx'
        megaminx_scramble
      when 'pyraminx'
        pyraminx_scramble
      when 'square-1'
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
      end.join(' ')
    end
    
    def megaminx_scramble
      scramble = ''
      turns = %w(R D)
      variants = %w(-- ++)
      variants_for_u = ['\'', '']
      scramble_length.times do |index|
        scramble += (scramble.empty? ? '' : ' ') + turns[index % 2] + variants.rand
        scramble += ' U' + variants_for_u.rand + "<br/>" if index % 10 == 9
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
    	up_layer = (0..7).map{|i| i%2 == 0 ? 30 : 60}
    	down_layer = [up_layer].flatten!
    	length = 0
      begin
        up_moves = possible_moves up_layer
        down_moves = possible_moves down_layer
        up_move = up_moves.rand
        down_moves.delete 0 if up_move == 0
        down_move = down_moves.rand
        scramble << [humanize_sq_one_move(up_layer, up_move), humanize_sq_one_move(down_layer, down_move) * -1]
        do_move up_layer, up_move
        do_move down_layer, down_move
        length += up_move == 0 ? 0 : 1
        length += down_move == 0 ? 0 : 1
        do_slice(up_layer, down_layer)
        length += 1
      end while length <= scramble_length + 1
      scramble.map {|s| "(#{s.join(',')})"}.join(' ')
    end
    
    def humanize_sq_one_move(layer, move)
      move = layer[0..move - 1].inject(0){|sum, x| x == 30 ? sum + 1 : sum + 2} unless move == 0
      move > 6 ? move - 12 : move
    end
    
    def possible_moves(layer)
      layer_moves = []
      layer.length.times do |start|
        sum = 0
        possible = false
        layer.length.times do |i|
          sum += layer[(start + i) % layer.length]
          possible = true if sum == 180
        end
        layer_moves << start if possible
      end
      layer_moves
    end
    
    def do_move(layer, l)
      l %= layer.length
      l.times do
        layer << layer.shift
      end
      layer
    end

    def do_slice(up, down)
      sum = 0
      small_up, big_up = up.partition {|n| sum += n; sum <= 180}
      small_down, big_down = down.partition {|n| sum += n; sum <= 540}
      up.replace(small_down.reverse + big_up)
      down.replace(small_up.reverse + big_down)
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
    	scramble.join(' / ')
    end
end
class Puzzle < ActiveRecord::Base
  belongs_to :kind, :order => 'name'
  belongs_to :record
  has_many :records, :conditions => ['record = ?', true], :order => 'time', :class_name => 'Clock'
  
  file_column :image, :store_dir => 'public/images/puzzles', :base_url => 'images/puzzles'
  
  validates_presence_of :name, :image, :kind_id
  validates_length_of :name, :maximum => 64
  validates_filesize_of :image, :in => 0..10.kilobytes
  validates_file_format_of :image, :in => ['gif']
  
  def scramble
    case name
      when '2x2x2', '3x3x3'
        cube_scramble [%w{R L}, %w{F B}, %w{D U}]
      when '4x4x4', '5x5x5'
        cube_scramble [%w{R L}, %w{F B}, %w{D U}, %w{r l}, %w{f b}, %w{d u}]
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
      (0..scramble_length).map do
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
        scramble += ' Y' + variants.rand if index % 10 == 9
      end
      scramble
    end
    
    def pyraminx_scramble
      turns = %w(U L R B)
      variants = ['', "'", '2']
      tip_turns = turns.map &:downcase
      tip_length = rand(3) + 2
      scramble = (0..tip_length).map do
        tip_turns.delete(tip_turns.rand) + variants.rand
      end
      axis = rand turns.size
      scramble += (tip_length..scramble_length).map do
        axis = (axis + rand(turns.size - 1) + 1) % turns.size
        turns[axis] + variants.rand
      end
      scramble.join(" ")
    end
    
    def square1_scramble
    	"blub"
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
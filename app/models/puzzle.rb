class Puzzle < ActiveRecord::Base
  belongs_to :kind, :order => :name
  
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
        cube_scramble [%w{R L r l}, %w{F B f b}, %w{D U d u}]
      when 'megaminx'
        megaminx_scramble
    end
  end
  
  private
    def cube_scramble(turns)
      scramble = ''
      variants = ['', "'", '2']
      axis = rand turns.size
      scramble_length.times do
        axis = (axis + rand(turns.size - 1) + 1) % turns.size
        scramble += (scramble.empty? ? '' : ' ') + turns[axis].rand + variants.rand
      end
      scramble
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
end
class Puzzle < ActiveRecord::Base
  FORMATS = %w{average mean best_of}

  belongs_to :kind
  has_many :competitions, :dependent => :destroy
  has_many :matches, :dependent => :destroy
  has_many :singles, :dependent => :destroy
  has_many :records, :order => :time, :include => :user, :conditions => { 'users.ignored' => false }, :dependent => :destroy do
    def amount(n)
      where(:amount => n)
    end
  end

  has_attached_file :image, :styles => { :combined => '' }, :processors => [:combined]
  #attr_protected :image_file_name, :image_content_type, :image_size

  validates_presence_of :name, :image, :attempt_count, :countdown, :kind_id
  validates_length_of :name, :maximum => 64
  validates_numericality_of :scramble_length, :greater_than_or_equal_to => 0, :less_than_or_equal_to => 255, :only_integer => true
  validates_numericality_of :countdown, :greater_than_or_equal_to => 0, :only_integer => true
  validates_numericality_of :attempt_count, :greater_than => 0, :only_integer => true
  validates_inclusion_of :average_format, :in => FORMATS
  validates_attachment_size :image, :less_than => 20.kilobytes, :unless => Proc.new { |puzzle| puzzle.image_file_name.blank? }
  validates_attachment_content_type :image, :content_type => ['image/png', 'image/gif'], :unless => Proc.new { |puzzle| puzzle.image_file_name.blank? }

  def self.default
    where('puzzles.name' => '3x3x3').joins(:kind).where('kinds.name' => 'speed').first.try(:id) || 1
  end

  def scrambles
    (1..attempt_count).map { |i| scramble }
  end

  def scramble
    case name.downcase
      when '3x3x3'
        Scrambler::ThreeByThree.new.scramble(attempt_count)
      when '4x4x4'
        Scrambler::FourByFour.new.scramble(attempt_count)
      when '5x5x5'
        Scrambler::FiveByFive.new.scramble(attempt_count)
      when '6x6x6'
        Scrambler::SixBySix.new.scramble(attempt_count)
      when '7x7x7'
        Scrambler::SevenBySeven.new.scramble(attempt_count)
      when 'megaminx'
        Scrambler::Megaminx.new.scramble(attempt_count)
      when 'pyraminx'
        Scrambler::Pyraminx.new.scramble(attempt_count)
      when 'square-1'
      	Scrambler::Square1.new.scramble(attempt_count)
      when 'clock'
      	Scrambler::Clock.new.scramble(attempt_count)
    	else
    	  ''
    end.html_safe
  end
end

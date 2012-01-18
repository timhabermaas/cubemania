class Single < ActiveRecord::Base
  belongs_to :user, :counter_cache => true
  belongs_to :puzzle

  attr_accessible :time, :human_time, :puzzle_id, :scramble, :penalty

  validates_presence_of :user_id, :puzzle_id, :time
  validates_inclusion_of :penalty, :in => %w( plus2 dnf ), :allow_nil => true

  before_validation :set_time, :if => :human_time_is_set
  after_destroy :update_records
  after_update :update_records

  scope :not_dnf, where("penalty IS NULL OR penalty NOT LIKE 'dnf'")
  scope :recent, lambda { |amount| order("created_at desc").limit(amount) }
  scope :for_user_and_puzzle, lambda { |user, puzzle| where(:user_id => user.id, :puzzle_id => puzzle.id) }

  humanize :time => :time

  # TODO check for correct format. option 1: validates_format_of + before_save, option 2: don't know, option 3: don't care at all
  def human_time=(ht)
    @human_time = ht
  end

  def toggle_dnf!
    if self.dnf?
      self.update_attribute :penalty, nil
    elsif self.plus2?
      self.update_attributes :penalty => "dnf", :time => self.time - 2000
    else
      self.update_attribute :penalty, "dnf"
    end
  end

  def toggle_plus2!
    if self.plus2?
      self.update_attributes :penalty => nil, :time => self.time - 2000
    else
      self.update_attributes :penalty => "plus2", :time => self.time + 2000
    end
  end

  def dnf?
    self.penalty == "dnf"
  end

  def plus2?
    self.penalty == "plus2"
  end

private
  def set_time
    seconds, minutes, hours = @human_time.split(':').reverse
    self.time = (hours.to_i * 3600 + minutes.to_i * 60) * 1000 + (seconds.to_f * 1000).to_i
  end

  def human_time_is_set
    not @human_time.blank?
  end

  def update_records
    #Record.calculate_for!(user_id, puzzle_id, 1)
    #Record.calculate_for!(user_id, puzzle_id, 5)
    #Record.calculate_for!(user_id, puzzle_id, 12)
  end
end

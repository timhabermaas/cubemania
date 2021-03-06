class Single < ActiveRecord::Base
  include ComparableSolve
  extend Humanizeable

  belongs_to :user, :counter_cache => true, :touch => true
  belongs_to :puzzle
  has_and_belongs_to_many :records

  attr_accessible :time, :puzzle_id, :scramble, :penalty, :comment

  validates_presence_of :user_id, :puzzle_id, :time
  validates_inclusion_of :penalty, :in => %w( plus2 dnf ), :allow_nil => true
  validates_length_of :comment, :maximum => 255

  before_validation :set_blank_penalty_to_nil
  after_update :reset_cached_record_comments

  scope :not_dnf, where("penalty IS NULL OR penalty NOT LIKE 'dnf'")
  scope :recent, lambda { |amount| order("created_at desc").limit(amount) }
  scope :for_user_and_puzzle, lambda { |user, puzzle| where(:user_id => user.id, :puzzle_id => puzzle.id) }
  scope :last_24_hours, lambda { where "created_at > ?", 24.hours.ago }
  scope :grouped, lambda { |options|
    raise ArgumentError, "by must be either :day, :week or :month" unless [:day, :week, :month].include? options[:by].to_sym

    field = if options[:time_zone]
      offset = ActiveSupport::TimeZone.new(options[:time_zone]).formatted_offset[1..-1]
      "date_trunc('#{options[:by]}', created_at + time '#{offset}')"
    else
      "date_trunc('#{options[:by]}', created_at)"
    end
    group(field).select(field + " as created_at, AVG(time) as time, string_agg(comment, '\n') as comment")
  }

  humanize :time => :time

  def created_at_timestamp
    self.created_at.to_i
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
  def set_blank_penalty_to_nil
    self.penalty = nil if self.penalty.blank?
  end

  def reset_cached_record_comments
    records.each(&:update_comment!) if comment_changed?
  end
end

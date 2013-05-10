class Record < ActiveRecord::Base
  extend Humanizeable

  serialize :single_ids, Array

  belongs_to :puzzle
  belongs_to :user, :touch => true

  validates_presence_of :user_id, :puzzle_id, :time, :amount, :singles, :set_at
  validates_inclusion_of :amount, :in => RecordType.counts
  validates_length_of :comment, :maximum => 255
  validate :has_as_many_singles_as_amount

  # FIXME creates a new scope which is propably very confusing
  def self.recent
    subquery = select("user_id, MIN(time) AS time, puzzle_id, amount").group("user_id, puzzle_id, amount")
    Record.where("(user_id, time, puzzle_id, amount) IN (#{subquery.to_sql})")
  end

  before_validation :set_set_at, :set_comments_from_singles

  humanize :time => :time

  def self.younger_than(single)
    Record.where(:user_id => single.user_id).
           where(:puzzle_id => single.puzzle_id).
           where("set_at >= ?", single.created_at)
  end

  def self.build_from_singles_and_type_and_time(singles, type, time)
    Record.new :time => time,
               :singles => singles,
               :user_id => singles.first.user_id,
               :puzzle_id => singles.first.puzzle_id,
               :amount => type.count
  end

  def self.grouped_by_puzzle_and_amount
    grouped_by_puzzles = recent.includes(:puzzle => :kind).group_by { |r| r.puzzle }
    grouped_by_puzzles.merge(grouped_by_puzzles) { |k, v| v = v.group_by { |r| r.amount }; v.merge(v) { |k, v| v.try(:first) } }
  end

  def singles
    @singles ||= Single.where(:id => single_ids).to_a
  end

  def singles=(singles)
    sorted_singles = (singles || []).sort_by(&:created_at)
    self.single_ids = sorted_singles.map(&:id)
    @singles = sorted_singles
  end

  def type
    RecordType.by_count amount
  end

  def type_full_name
    type.full_name
  end

  def update_comment!
    update_attributes :comment => comments_from_singles
  end

  private
  def has_as_many_singles_as_amount
    errors.add(:singles, "must have #{amount} items, but has #{singles.size}") if singles && singles.size != amount
  end

  def set_set_at
    unless singles.empty?
      self.set_at = singles.sort_by { |s| s.created_at }.last.created_at || Time.now
    end
  end

  def comments_from_singles
    singles.map(&:comment).uniq.reject(&:blank?).join("; ")[0..254]
  end

  def set_comments_from_singles
    self.comment = comments_from_singles
  end
end

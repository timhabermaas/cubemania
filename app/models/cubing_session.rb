class CubingSession < ActiveRecord::Base
  serialize :single_ids, Array
  belongs_to :puzzle
  belongs_to :user

  validates_presence_of :user, :puzzle

  def self.create_from_single(single)
    create :user_id => single.user_id,
           :puzzle_id => single.puzzle_id,
           :singles => [single]
  end

  def self.for(user_id, puzzle_id)
    where(:user_id => user_id, :puzzle_id => puzzle_id)
  end

  def self.last_for(user_id, puzzle_id)
    self.for(user_id, puzzle_id).order("updated_at desc").first
  end

  def singles
    Single.where(:id => single_ids)
  end

  def singles=(singles)
    self.single_ids = singles.map(&:id)
  end

  def add_single(single)
    single_ids << single.id
  end

  def add_single!(single)
    add_single(single)
    save
  end

  def count
    single_ids.size
  end

  def from
    singles.first.created_at
  end

  def to
    singles.last.created_at
  end

  # TODO alias Single#created_at to stopped_at?
  def too_old?(single, time_span=6.hours)
    single.created_at - singles.last.created_at > time_span
  end

  def new?
    single_ids.size <= 1
  end
end

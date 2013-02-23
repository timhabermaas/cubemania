class CubingSession < ActiveRecord::Base
  serialize :single_ids, Array
  belongs_to :puzzle
  belongs_to :user

  validates_presence_of :user, :puzzle

  def singles
    Single.where(:id => single_ids)
  end

  def singles=(singles)
    self.single_ids = singles.map(&:id)
  end

  def add_single(single)
    single_ids << single.id
  end
end

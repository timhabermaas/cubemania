class AverageRecord < ActiveRecord::Base
  belongs_to :user
  belongs_to :puzzle

  validates_presence_of :user_id, :puzzle_id, :time, :single_ids
  validates_uniqueness_of :user_id, :scope => :puzzle_id

  def singles=(s)
    self.single_ids = s.map(&:id).join(";")
  end

  def singles
    Single.find(single_ids.split(";"))
  end

  def human_time(spacer = '')
    t = self.time || 0
    hs = (t / 10.0).round
    if hs >= 6000
      min = hs / 6000
      sec = (hs - min * 6000) / 100.0
      '%d:%05.2f' % [min, sec] + spacer + 'min' # 12.555 => "12.55"
    else
      '%.2f' % (hs.to_f / 100) + spacer + 's'
    end
  end
end

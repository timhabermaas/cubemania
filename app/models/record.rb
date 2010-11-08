class Record < ActiveRecord::Base
  belongs_to :user
  belongs_to :puzzle

  validates_presence_of :user_id, :puzzle_id, :time, :amount, :single_ids
  validates_uniqueness_of :user_id, :scope => [:puzzle_id, :amount]
  validates_inclusion_of :amount, :in => [1, 5, 12]

  humanize :time => :time

  def singles=(s)
    self.single_ids = s.map(&:id).join(';')
  end

  def singles
    Single.find(single_ids.split(';'))
  end

  def self.update_for(user_id, puzzle_id)
    [5, 12].each do |amount|
      record = Record.find_by_user_id_and_puzzle_id_and_amount(user_id, puzzle_id, amount)
      output = `bin/average #{user_id} #{puzzle_id} #{amount}`

      if output == "NULL"
        record.try(:destroy)
        next
      end

      t, *singles = output.split(',').map { |v| v.to_i }
      single_ids = singles.join(';')

      if record.nil?
        Record.create!(:user_id => user_id, :puzzle_id => puzzle_id, :single_ids => single_ids, :amount => amount, :time => t)
      else
        record.update_attributes(:time => t, :single_ids => single_ids)
      end
    end
  end
end

class Single < ActiveRecord::Base
  belongs_to :user, :counter_cache => true
  belongs_to :puzzle

  validates_presence_of :user_id, :puzzle_id, :time

  after_save :check_for_new_record

private
  def check_for_new_record
    # get the last 5 solves, calculate an average. if the average is lower than user.average_records.for(puzzle_id).time
  end
end

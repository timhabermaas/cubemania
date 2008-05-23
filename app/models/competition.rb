class Competition < ActiveRecord::Base
  REPEATS = %w{once daily weekly monthly}

  belongs_to :puzzle
  has_many :averages, :include => :user, :order => 'time', :dependent => :nullify
  has_many :singles, :dependent => :nullify
  belongs_to :user; attr_protected :user_id, :user

  validates_presence_of :name, :repeat, :user_id

  def participated?(user)
    averages.collect { |a| a.user }.include? user
  end

  def started_at
    if repeat == 'once'
      created_at
    else
      Time.now.send "beginning_of_#{nominalize_repeat}"
    end
  end

  def ends_at
    if repeat == 'once'
      created_at.next_month
    else
      Time.now.send "end_of_#{nominalize_repeat}"
    end
  end

  private
    def nominalize_repeat
      repeat == 'daily' ? 'day' : repeat[0..-3]
    end
end
class Competition < ActiveRecord::Base
  REPEATS = %w{once daily weekly monthly}

  belongs_to :puzzle
  has_many :averages, :include => :user, :order => 'time', :dependent => :nullify do
    def current(competition); self.for competition; end
    def for(competition, time = Time.now)
      find :all, :conditions => ['created_at >= ? and created_at < ?', competition.started_at(time), competition.ended_at(time)]
    end
  end
  has_many :singles, :dependent => :nullify
  belongs_to :user; attr_protected :user_id, :user

  validates_presence_of :name, :repeat, :user_id

  def participated?(user)
    averages.collect { |a| a.user }.include? user
  end

  def started_at(time = Time.now)
    if repeat == 'once'
      created_at
    else
      time.send "beginning_of_#{nominalize_repeat}"
    end
  end

  def ended_at(time = Time.now)
    if repeat == 'once'
      created_at.next_month
    else
      time.send "end_of_#{nominalize_repeat}"
    end
  end

  private    
    def nominalize_repeat
      repeat == 'daily' ? 'day' : repeat[0..-3]
    end
end
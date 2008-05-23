class Competition < ActiveRecord::Base
  REPEATS = %w{once daily weekly monthly}

  belongs_to :puzzle
  has_many :averages, :include => :user, :order => 'time', :dependent => :nullify do
    def for(competition, date)
      find :all, :conditions => ['created_at >= ? and created_at < ?', competition.started_at(date), competition.ended_at(date)]
    end
  end
  has_many :singles, :dependent => :nullify
  belongs_to :user; attr_protected :user_id, :user

  validates_presence_of :name, :repeat, :user_id

  def participated?(user)
    averages.collect { |a| a.user }.include? user
  end

  def started_at(date)
    if repeat == 'once'
      created_at
    else
      date.send "beginning_of_#{nominalize_repeat}"
    end
  end

  def ended_at(date)
    if repeat == 'once'
      created_at.next_month
    else
      date.send "end_of_#{nominalize_repeat}"
    end
  end
  
  def previous?(date)
    started_at(date) != started_at(created_at)
  end
  
  def previous_date(date)
    started_at date.ago 1.send(nominalize_repeat)
  end
  
  def next?(date)
    ended_at(date) != ended_at(Time.now)
  end
  
  def next_date(date)
    started_at date.in 1.send(nominalize_repeat)
  end

  def old?(date)
    started_at(date) != started_at(Time.now)
  end

  private    
    def nominalize_repeat
      repeat == 'daily' ? 'day' : repeat[0..-3]
    end
end
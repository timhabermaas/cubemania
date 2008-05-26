class Competition < ActiveRecord::Base
  REPEATS = %w{once daily weekly monthly}

  belongs_to :puzzle
  has_many :averages, :include => :user, :order => 'time', :dependent => :nullify do
    def for(competition, date)
      find :all, :conditions => ['created_at between ? and ?', competition.started_at(date), competition.ended_at(date)]
    end
  end
  has_many :singles, :dependent => :nullify
  belongs_to :user; attr_protected :user_id, :user

  validates_presence_of :name, :repeat, :user_id
  validates_length_of :name, :in => 2..64
  validates_length_of :description, :maximum => 256, :allow_nil => true
  validates_inclusion_of :repeat, :in => REPEATS

  def participated?(date, user)
    averages.for(self, date).collect { |a| a.user }.include? user
  end

  def started_at(date = Time.now)
    if repeat == 'once'
      created_at
    else
      date.send "beginning_of_#{nominalize_repeat}"
    end
  end

  def ended_at(date = Time.now)
    if repeat == 'once'
      created_at.next_year
    else
      date.send "end_of_#{nominalize_repeat}"
    end
  end
  
  def previous?(date)
    started_at(date) != started_at(created_at)
  end
  
  def previous_date(date)
    started_at date.ago(1.send(nominalize_repeat))
  end
  
  def next?(date)
    ended_at(date) != ended_at(Time.now)
  end
  
  def next_date(date)
    started_at date.in(1.send(nominalize_repeat))
  end

  def old?(date)
    started_at(date) != started_at(Time.now)
  end
  
  def once?
    repeat == 'once'
  end

  private    
    def nominalize_repeat
      repeat == 'daily' ? 'day' : repeat[0..-3]
    end
end
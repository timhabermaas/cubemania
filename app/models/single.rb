class Single < Clock
  belongs_to :average
  
  validates_presence_of :position
  
  def comment
    average.comment unless average.nil?
  end
end
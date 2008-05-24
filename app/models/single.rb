class Single < Clock
  belongs_to :average
  
  def comment
    average.comment unless average.nil?
  end
end
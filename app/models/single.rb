class Single < Clock
  belongs_to :average
  
  def comment
    average.comment
  end
end
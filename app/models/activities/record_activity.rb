class RecordActivity < Activity
  belongs_to :trackable, :class_name => "Record"

  def corrupt?
    super or trackable.updated_at > self.created_at
  end
end

class RecordActivity < Activity
  belongs_to :trackable, :class_name => "Record"
end

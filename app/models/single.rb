class Single < Clock
  belongs_to :average
  belongs_to :user; attr_protected :user_id, :user
end
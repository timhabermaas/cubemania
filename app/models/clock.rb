class Clock < ActiveRecord::Base
  belongs_to :user; attr_protected :user_id, :user
  belongs_to :puzzle
end
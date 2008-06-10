class Shout < ActiveRecord::Base
  belongs_to :competition
  belongs_to :user
  
  validates_presence_of :content, :user_id, :competition_id
end

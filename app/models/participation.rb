class Participation < ActiveRecord::Base
  belongs_to :user
  belongs_to :competition
  
  validates_uniqueness_of :user_id, :scope => :competition_id
end

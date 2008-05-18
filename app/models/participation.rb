class Participation < ActiveRecord::Base
  belongs_to :user; attr_protected :user_id, :user
  belongs_to :competition

  validates_presence_of :competition_id, :user_id
  validates_uniqueness_of :user_id, :scope => :competition_id
end
class Authorization < ActiveRecord::Base
  belongs_to :user

  validates :user_id, :provider, :uid, :presence => true
  validates_uniqueness_of :provider, :scope => :user_id
end

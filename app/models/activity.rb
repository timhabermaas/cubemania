class Activity < ActiveRecord::Base
  belongs_to :user
  belongs_to :trackable

  def corrupt?
    trackable.nil?
  end
end

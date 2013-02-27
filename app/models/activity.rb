class Activity < ActiveRecord::Base
  belongs_to :user
  belongs_to :trackable, :polymorphic => true
  has_many :comments, :as => :commentable, :dependent => :destroy

  def corrupt?
    trackable.nil?
  end

  def sti_parent
    becomes(Activity)
  end
end

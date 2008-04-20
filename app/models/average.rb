class Average < Clock
  belongs_to :user, :counter_cache => true; attr_protected :user_id, :user
  has_many :singles, :order => 'time', :dependent => :destroy
end
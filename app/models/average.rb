class Average < Clock
  has_many :singles, :order => 'time', :dependent => :destroy
end
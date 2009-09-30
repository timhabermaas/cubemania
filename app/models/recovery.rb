class Recovery
  attr_accessor :email
  def initialize(attributes = nil)
    if attributes
      @email = attributes[:email]
    end
    @errors = ActiveRecord::Errors.new(self)
  end
  
  def errors
    @errors
  end
  
  def validate
    user = User.find_by_email @email
    errors.add :email, "doesn't exist" if user.nil? 
    user
  end
  
  def self.self_and_descendants_from_active_record
    [self]
  end
  
  def self.human_attribute_name(attr)
    attr.to_s.titleize
  end
  
  def self.human_name
    'Recovery'
  end
end
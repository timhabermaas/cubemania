require 'active_record/validations'

class Session
  attr_accessor :name, :password

  def initialize(attributes = nil)
    unless attributes.nil?
      self.name = attributes[:name]
      self.password = attributes[:password]
    end
    @errors = ActiveRecord::Errors.new self
  end

  def validate
    user = User.find_by_name_and_password name, password
    errors.add :password, 'does not match name' if user.nil?
    self.password = nil
    user
  end

  def id
    nil
  end

  def errors
    @errors
  end
  
  def new_record?
    true
  end
  
  def self.human_attribute_name(attr)
    attr.to_s.titleize
  end
end
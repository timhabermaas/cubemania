class Login
  include ActiveModel::Conversion
  include ActiveModel::Naming
  include ActiveModel::Validations

  attr_accessor :name, :password

  def initialize(attributes = nil)
    if attributes
      @name = attributes[:name]
      @password = attributes[:password]
    end
  end

  def validate
    user = User.authorize name, password
    errors.add :password, 'does not match name' if user.nil?
    self.password = nil
    user
  end

  def persisted?
    false
  end
end

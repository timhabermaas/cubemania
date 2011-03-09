class Login
  include ActiveModel::Naming
  include ActiveModel::Validations

  attr_accessor :name, :password

  def validate
    user = User.find_by_name_and_password name, password
    errors.add :password, 'does not match name' if user.nil?
    self.password = nil
    user
  end

  def initialize(attributes = nil)
    if attributes
      @name = attributes[:name]
      @password = attributes[:password]
    end
  end
end
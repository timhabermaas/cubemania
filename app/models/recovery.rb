class Recovery
  include ActiveModel::Naming
  include ActiveModel::Validations

  attr_accessor :email

  def initialize(attributes = nil)
    if attributes
      @email = attributes[:email]
    end
  end

  def validate
    user = User.find_by_email @email
    errors.add :email, "doesn't exist" if user.nil?
    user
  end
end
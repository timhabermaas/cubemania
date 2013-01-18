module ApiHelper
  include Rack::Test::Methods

  def app
    Rails.application
  end
end

RSpec.configure do |c|
  c.include ApiHelper # TODO only include when :type => :api.
                      # It's either broken or I'm stupid...
end

module ApiHelper
  include Rack::Test::Methods

  def app
    Rails.application
  end

  def json_response
    JSON.parse last_response.body
  end
end

RSpec.configure do |c|
  c.include ApiHelper # TODO only include when :type => :api.
                      # It's either broken or I'm stupid...
end

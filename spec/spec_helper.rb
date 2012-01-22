require "spork"

Spork.prefork do
  ENV["RAILS_ENV"] ||= "test"
  require File.expand_path("../../config/environment", __FILE__)
  require "rspec/rails"

  Dir[Rails.root.join("spec/support/**/*.rb")].each {|f| require f}

  RSpec.configure do |config|
    config.mock_with :rspec

    config.use_transactional_fixtures = true

    config.include FactoryGirl::Syntax::Methods
    config.include Capybara::SessionHelper

    Capybara.add_selector(:li) do
      xpath { |num| ".//li[#{num}]" }
    end
  end
end

Spork.each_run do
  FactoryGirl.factories.clear
  FactoryGirl.reload
  load File.expand_path("../../config/routes.rb", __FILE__)
end
ENV['CUBEMANIA_DATABASE'] = Cubemania::Application.config.database_configuration[Rails.env]["database"]
ENV['CUBEMANIA_USERNAME'] = Cubemania::Application.config.database_configuration[Rails.env]["username"]
ENV['CUBEMANIA_PASSWORD'] = Cubemania::Application.config.database_configuration[Rails.env]["password"]
ENV['CUBEMANIA_DATABASE'] = Cubemania::Application.config.database_configuration[RAILS_ENV]["database"]
ENV['CUBEMANIA_USERNAME'] = Cubemania::Application.config.database_configuration[RAILS_ENV]["username"]
ENV['CUBEMANIA_PASSWORD'] = Cubemania::Application.config.database_configuration[RAILS_ENV]["password"]
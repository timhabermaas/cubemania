ENV['DB_DATABASE'] = Cubemania::Application.config.database_configuration[RAILS_ENV]["database"]
ENV['DB_USERNAME'] = Cubemania::Application.config.database_configuration[RAILS_ENV]["username"]
ENV['DB_PASSWORD'] = Cubemania::Application.config.database_configuration[RAILS_ENV]["password"]
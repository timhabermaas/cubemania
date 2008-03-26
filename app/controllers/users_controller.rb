class UsersController < ApplicationController
  login :role => :self, :only => [:edit, :update, :destroy]
  logout :only => [:new, :create]

  resource_controller

  create.after { self.user = object }
  destroy.after { self.user = nil }
end
class UsersController < ApplicationController
  login :only => [:edit, :update, :destroy]
  logout :only => [:new, :create]

  resource_controller

  create.after { self.user = object }
  destroy.after { self.user = nil }
end
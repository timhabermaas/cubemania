class CommentsController < ResourceController::Base
  login
  
  belongs_to :user
end
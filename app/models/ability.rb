class Ability
  include CanCan::Ability

  def initialize(user)
    user ||= User.new
    if user.admin?
      can :manage, :all
    else
      can :read, :all
      can :create, Comment
      can :destroy, Comment do |comment|
        comment.user == user || user.admin?
      end
    end
  end
end
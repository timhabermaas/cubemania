class Ability
  include CanCan::Ability

  def initialize(user)
    can :read, :all
    if user.nil?
      can :create, User
    elsif user.admin?
      can :manage, :all
    else
      can :update, User do |u|
        u == user
      end
      can :create, Comment
      can :create, Single
      can [:destroy, :dnf, :plus2], Single do |single|
        single.user == user
      end
      can :destroy, Comment do |comment|
        comment.user == user
      end
    end
  end
end
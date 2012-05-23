class Ability
  include CanCan::Ability

  def initialize(user)
    can :read, :all
    if user.nil?
      can :create, User
    elsif user.admin?
      can :manage, :all
    else
      can [:destroy, :update], User do |u|
        u == user
      end
      can :share, Record do |record|
        record.user_id == user.id
      end
      can :create, Comment
      can :create, Competition
      can :create, Shout
      can :compete, Competition
      can :update, Competition do |competition|
        competition.user_id == user.id
      end
      can :destroy, [Comment, Shout] do |comment|
        comment.user_id == user.id
      end
      cannot :read, Puzzle
    end
  end
end

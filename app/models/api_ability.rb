# TODO move to app/abilities

class ApiAbility
  include CanCan::Ability

  def initialize(user)
    can :read, :all

    if user.nil?
    elsif user.admin?
      can :manage, :all
    else
      can :create, Single

      can [:update, :destroy], Single do |s|
        s.user_id == user.id
      end

      if user.moderator?
        can :block, User
      end
    end
  end
end

class UserSweeper < ActionController::Caching::Sweeper
  observe User

  # Hack, cause rails is broken
  def before(controller)
    self.controller = controller
    if controller.perform_caching
      callback(:before)
    else
      true
    end
  end

  def after_destroy(user)
    flush_users_page
  end

  def after_save(user)
    flush_users_page if user.name_changed?
  end

private
  def flush_users_page
    Rails.cache.delete("views/users")
  end
end
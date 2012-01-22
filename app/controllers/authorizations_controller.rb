class AuthorizationsController < ApplicationController
  skip_load_and_authorize_resource

  def create
    provider, uid = request.env["omniauth.auth"][:provider], request.env["omniauth.auth"][:uid]
    credentials = request.env["omniauth.auth"][:credentials]

    authorization = current_user.authorizations.find_by_provider provider
    if authorization
      authorization.update_attributes :uid => uid, :token => credentials[:token], :secret => credentials[:secret]
      redirect_to authorizations_path, :notice => "Successfully connected to #{provider.humanize}."
    else
      authorization = current_user.authorizations.build :provider => provider,
                                                        :uid => uid,
                                                        :token => credentials[:token],
                                                        :secret => credentials[:secret]
      if authorization.save
        redirect_to authorizations_path, :notice => "Successfully connected to #{provider.humanize}."
      else
        redirect_to authorizations_path, :notice => "Couldn't connect accounts."
      end
    end

  rescue
    redirect_to authorizations_path, :notice => "Couldn't connect accounts."
  end

  def failure
    redirect_to authorizations_path, :alert => "Couldn't connect accounts. Error: \"#{params[:message].humanize}\""
  end

  def index
    @authorizations = current_user.authorizations
  end

  def destroy
    authorization = current_user.authorizations.find params[:id]
    authorization.destroy
    redirect_to authorizations_path, :notice => "Successfully disconnected from #{authorization.provider.humanize}."
  end
end

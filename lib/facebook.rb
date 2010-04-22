module Facebook
  class Client
    include HTTParty
    base_uri "https://graph.facebook.com"
    format :json

    def initialize(access_token)
      @access_token = access_token
    end

    def get(path, options = {})
      self.class.get(path, :query => options.merge( { :access_token => @access_token } ) )
    end

    def post(path, options = {})
      self.class.post(path, :body => options.merge( { :access_token => @access_token } ) )
    end
  end

  class Consumer
    include HTTParty
    base_uri "https://graph.facebook.com"
    format :plain

    def initialize(api_key, api_secret, options = {})
      @api_key, @api_secret = api_key, api_secret
      @redirect_uri = options[:redirect_uri]
      if options[:scope]
        @scope = options[:scope].is_a?(Array) ? options[:scope].join(',') : options[:scope]
      end
    end

    def authorize!(code)
      params = {
                 :client_id => @api_key,
                 :client_secret => @api_secret,
                 :redirect_uri => @redirect_uri,
                 :code => code
               }
      self.class.get('oauth/access_token', :query => params)[13..-1] # yes, that's a hack.
    end

    def authorize_url
      params = {
                 :client_id => @api_key,
                 :redirect_uri => @redirect_uri,
                 :scope => @scope
               }.reject { |k,v| v.nil? }
      self.class.base_uri + '/oauth/authorize?' + params.to_params
    end
  end
end
# Your secret key for verifying cookie session data integrity.
# If you change this key, all old sessions will become invalid!
# Make sure the secret is at least 30 characters and all random, 
# no regular words or you'll be exposed to dictionary attacks.

ActionController::Base.session = {
  :session_key => 'beta_cubemania_session_id',
  :secret      => 'fjalfaoe11ngdf20sd9454q9fs9a2450123sdf01233129fssnlösaa',
  :expire_after => 30.days
}
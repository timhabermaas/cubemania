# Your secret key for verifying cookie session data integrity.
# If you change this key, all old sessions will become invalid!
# Make sure the secret is at least 30 characters and all random, 
# no regular words or you'll be exposed to dictionary attacks.

ActionController::Base.session = {
  :session_key => '_cubemania_session_id',
  :secret      => 'some s3cr3t phrase of at least 42 characters',
  :expire_after => 30.days
}
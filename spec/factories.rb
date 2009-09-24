Factory.define :user do |u|
  u.name 'tim'
  u.password 'some_password'
  u.password_confirmation 'some_password'
  u.email 'foo@bar.com'
  u.bot_email ''
end

Factory.define :admin, :class => User do |u|
  u.name 'admin'
  u.password 'some_password'
  u.password_confirmation 'some_password'
  u.email 'foo@bar2.com'
  u.role 'admin'
  u.bot_email ''
end

Factory.define :kind do |k|
  k.name 'blindfolded'
  k.image 'some.jpg'
end

Factory.define :puzzle do |p|
  p.name '3x3x3'
  p.scramble_length 25
  p.attempts_count 5
  p.association :kind
end

Factory.define :single do |s|
  s.time rand(16000) + 5000
  s.dnf false
  s.association :puzzle
end
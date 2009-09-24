Factory.define :user do |u|
  u.sequence(:name) { |n| "tim#{n}"}
  u.password 'some_password'
  u.password_confirmation { |u| u.password }
  u.sequence(:email) { |n| "foo#{n}@bar.com" }
  u.bot_email ''
  u.role 'user'
end

Factory.define :admin, :parent => :user do |u|
  u.role 'admin'
end

Factory.define :post do |p|
  p.sequence(:title) { |n| "title #{n}" }
  p.content 'some stupid content full of stuff'
  p.association(:user)
end

Factory.define :kind do |k|
  k.name 'blindfolded'
end

Factory.define :puzzle do |p|
  p.name '3x3x3'
  p.scramble_length 25
  p.attempt_count 5
  p.association :kind
end

Factory.define :single do |s|
  s.time rand(16000) + 5000
  s.dnf false
  s.record false
  s.association :puzzle
  s.association :user
  s.position 1
end

Factory.define :average do |a|
  a.time 21012
  a.dnf false
  a.record false
  a.association :puzzle
  a.association :user
  a.singles { |average| (1..5).to_a.map{|i| average.association(:single)} }
end
FactoryGirl.define do
  sequence(:name)       { |n| "muh#{n}" }
  sequence(:short_name) { |n| "muh#{n}" }

  factory :user do
    name
    password 'some_password'
    password_confirmation { |u| u.password }
    sequence(:email) { |n| "foo#{n}@bar.com" }
    bot_email ''
    role 'user'
    factory :admin do
      role 'admin'
    end
  end

  factory :competition do
    name "CubeMonster"
    description "Rule it!"
    user
    puzzle
    repeat "daily"
    skill "beginner"
  end

  factory :authorization do
    provider "facebook"
    uid "12325141asda"
    token "afaslga9airadsfa12111"
    user
  end

  factory :post do
    sequence(:title) { |n| "title #{n}" }
    content 'some stupid content full of stuff'
    association(:user)
  end

  factory :kind do
    name
    short_name
  end

  factory :puzzle do
    name
    scramble_length 25
    attempt_count 5
    association :kind
  end

  factory :single do
    time { rand(16000) + 5000 }
    association :puzzle
    association :user
  end
  factory :dnf_single, :parent => :single do
    penalty "dnf"
  end
  factory :plus2_single, :parent => :single do
    penalty "plus2"
  end

  factory :average do
    time { rand(16000) + 5000 }
    association :puzzle
    association :user
    association :competition
    singles { FactoryGirl.build_list(:single, 5) }
  end

  factory :record do
    time { rand(16000) + 5000 }
    association :puzzle
    association :user
    amount 5
    singles { FactoryGirl.build_list(:single, amount) }
    set_at Time.now
  end

  factory :match do
    association :user
    association :opponent, :factory => :user
    association :puzzle
  end
end

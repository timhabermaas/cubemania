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
  factory :cube, :parent => :puzzle do
    name "3x3x3"
    association :kind, :name => "speed", :short_name => ""
  end
  factory :cube_bld, :parent => :puzzle do
    name "3x3x3"
    association :kind, :name => "blindfolded", :short_name => "BLD"
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

  factory :record do
    time { rand(16000) + 5000 }
    association :puzzle
    association :user
    amount 5
    singles { FactoryGirl.build_list(:single, amount) }
    set_at Time.now
  end

  factory :cubing_session do
    association :user
    association :puzzle
    single_ids { FactoryGirl.create_list(:single, 4, :user => user, :puzzle => puzzle).map(&:id) }
  end

  factory :following do
    association :follower, :factory => :user
    association :followee, :factory => :user
  end

  factory :follow_activity do
    association :user
    association :trackable, :factory => :following
  end

  factory :record_activity do
    association :user
    association :trackable, :factory => :record
  end

  factory :cubing_session_activity do
    association :user
    association :trackable, :factory => :cubing_session
  end
end

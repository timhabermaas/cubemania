Delayed::Worker.delay_jobs = !(Rails.env.test? || Rails.env.development?)

silence_warnings do
  Delayed::Job.const_set("MAX_ATTEMPTS", 3)
end

require "spec_helper"

describe Competition do
  describe "validations" do
    it { should validate_presence_of :name }
    it { should validate_presence_of :skill }
    it { should validate_presence_of :repeat }
    it { should validate_presence_of :user_id }
    it { should validate_presence_of :puzzle_id }
  end
end

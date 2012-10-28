require "spec_helper"

describe "Users" do
  describe "GET /users/:id" do
    let(:puzzle)  { create :puzzle, :name => "3x3x3" }
    let(:puzzle2) { create :puzzle, :name => "4x4x4" }
    let(:user)    { create :user, :name => "rowe", :wasted_time => 14153 }

    it "displays the name and wasted time" do
      visit user_path(user)
      within("#content h1") do
        expect(page).to have_content "rowe"
        expect(page).to have_content "has spent about 4 hours solving puzzles"
      end
    end

    context "having records" do
      let!(:record)  { create :record, :user => user, :puzzle => puzzle, :amount => 5, :time => 14500 }
      let!(:record2) { create :record, :user => user, :puzzle => puzzle2, :amount => 12, :time => 22300 }

      it "displays his 3x3x3 record" do
        visit user_path(user)

        within("#records") do
          within("tr", :text => "3x3x3") do
            expect(find("td.avg5")).to have_content "14.50s"
          end
          within("tr", :text => "4x4x4") do
            expect(find("td.avg12")).to have_content "22.30s"
          end
        end
      end
    end
  end
end

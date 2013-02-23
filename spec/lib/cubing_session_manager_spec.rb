require_relative "../../lib/cubing_session_manager"

describe CubingSessionManager do
  describe ".create_or_add" do
    let(:session_class) { stub }
    let(:new_session) { stub }
    let(:single) { stub(:single, :user_id => 2, :puzzle_id => 10) }

    context "no existing session" do
      before { session_class.should_receive(:last_for).with(2, 10).and_return(nil) }

      it "creates a new session" do
        session_class.should_receive(:create_from_single)
                     .with(single)
                     .and_return(new_session)
        result = CubingSessionManager.create_or_add(single, session_class)
        expect(result).to eq new_session
      end
    end

    context "existing session" do
      let(:old_session) { stub }
      before { session_class.should_receive(:last_for).with(2, 10).and_return(old_session) }

      context "which is too old" do
        before { old_session.should_receive(:too_old?).with(single).and_return(true) }

        it "creates a new session" do
          session_class.should_receive(:create_from_single)
                       .with(single)
                       .and_return(new_session)
          result = CubingSessionManager.create_or_add(single, session_class)
          expect(result).to eq new_session
        end
      end

      context "which is young enough" do
        before { old_session.should_receive(:too_old?).with(single).and_return(false) }

        it "adds the single to the current session" do
          old_session.should_receive(:add_single!).with(single)
          result = CubingSessionManager.create_or_add(single, session_class)
          expect(result).to eq old_session
        end
      end
    end
  end
end
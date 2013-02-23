require_relative "../../lib/cubing_session_manager"

describe CubingSessionManager do
  let(:session_class) { stub }
  let(:single) { stub(:single, :user_id => 2, :puzzle_id => 10) }

  describe ".create_or_add" do
    let(:new_session) { stub }

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

  describe ".remove" do
    let(:session_1) { stub(:session, :singles => []) }
    let(:session_2) { stub(:session, :singles => [stub]) }
    let(:sessions) { [session_1, session_2] }

    before do
      session_class.should_receive(:for).with(2, 10).and_return(sessions)
    end

    it "" do
      session_1.should_receive(:destroy)
      CubingSessionManager.remove(single, session_class)
    end
  end
end

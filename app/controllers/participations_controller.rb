class ParticipationsController < ApplicationController
  def create
    @participation = current_user.participations.build params[:participation]
    @participation.save
    redirect_to kind_puzzle_competition_path params[:kind_id], params[:puzzle_id], @participation.competition_id
  end
  
  def destroy
    @participation = Participation.find params[:id]
    @participation.destroy
    redirect_to competitions_path
  end
end

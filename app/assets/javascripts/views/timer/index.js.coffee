class Cubemania.Views.TimerIndex extends Cubemania.BaseView
  template: JST["timer/index"]

  initialize: ->
    @bindTo @collection, "reset", @render, this
    @bindTo @collection, "add", @prependSingle, this
    @bindTo Cubemania.currentPuzzle, "change", @refetchSingles, this
    @statsView = new Cubemania.Views.Stats(singles: @collection, records: new Cubemania.Collections.Records())
    @timerView = new Cubemania.Views.Timer(collection: @collection)
    @chartView = new Cubemania.Views.Chart(collection: @collection)
    $(document).keydown(@timerView.stopTimer)
    $(document).keyup(@timerView.startTimer)

  render: ->
    $(@el).html(@template(singles: @collection))

    @timerView.setElement(@$("#timer")).render()
    @statsView.setElement(@$("#stats")).render()
    @chartView.setElement(@$("#chart-container")).render()

    @collection.each(@prependSingle)
    this

  prependSingle: (single) ->
    view = new Cubemania.Views.Single(model: single)
    @$("#singles ol").prepend(view.render().el)

  refetchSingles: (puzzle) ->
    @collection.setPuzzleId(puzzle.get("id"))
    @collection.fetch(data: {user_id: Cubemania.currentUser.get("id")}) # TODO use Cubemania.currentUser.fetchSingles instead?

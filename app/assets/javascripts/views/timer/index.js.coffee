class Cubemania.Views.TimerIndex extends Cubemania.BaseView
  template: JST["timer/index"]

  initialize: (options) ->
    @records = options.records
    @bindTo @collection, "reset", @render, this
    @bindTo Cubemania.currentPuzzle, "change", @refetchSingles, this
    @statsView = @addSubview new Cubemania.Views.Stats(singles: @collection, records: @records)
    @timerView = @addSubview new Cubemania.Views.Timer(collection: @collection)
    @chartView = @addSubview new Cubemania.Views.Chart(collection: @collection)
    @singlesView = @addSubview new Cubemania.Views.Singles(collection: @collection)
    $(document).ajaxComplete(@checkForNewRecord)

  render: ->
    $(@el).html(@template(singles: @collection))

    @timerView.setElement(@$("#timer")).render()
    @statsView.setElement(@$("#stats")).render()
    @chartView.setElement(@$("#chart-container")).render()
    @singlesView.setElement(@$("#singles")).render()

    this

  refetchSingles: (puzzle) ->
    @collection.setPuzzleId(puzzle.get("id"))
    @collection.fetch(data: {user_id: Cubemania.currentUser.get("id")}) # TODO use Cubemania.currentUser.fetchSingles instead?

  checkForNewRecord: (e, request, options) =>
    if request.getResponseHeader("X-NewRecord")
      @records.fetch(data: $.param(user_id: Cubemania.currentUser.get("id")))

  onDispose: ->
    $(document).unbind("ajaxComplete")

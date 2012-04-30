class Cubemania.Views.TimerIndex extends Cubemania.BaseView
  template: JST["timer/index"]

  initialize: (options) ->
    @records = options.records
    @bindTo Cubemania.currentPuzzle, "change", @refetchSingles, this
    @bindTo Cubemania.currentPuzzle, "change", @refetchRecords, this
    @statsView = @addSubview new Cubemania.Views.Stats(singles: @collection, records: @records)
    @timerView = @addSubview new Cubemania.Views.Timer(collection: @collection)
    @chartView = @addSubview new Cubemania.Views.Chart(collection: @collection)
    @singlesView = @addSubview new Cubemania.Views.Singles(collection: @collection)
    $(document).ajaxComplete(@checkForNewRecord)

  render: ->
    $(@el).html(@template())

    @timerView.setElement(@$("#timer")).render()
    @statsView.setElement(@$("#stats")).render()
    @chartView.setElement(@$("#chart-container")).render()
    @singlesView.setElement(@$("#singles")).render()

    this

  refetchSingles: (puzzle) ->
    @collection.setPuzzleId(puzzle.get("id"))
    @collection.fetch(data: {user_id: Cubemania.currentUser.get("id")}) # TODO use Cubemania.currentUser.fetchSingles instead?

  refetchRecords: (puzzle) ->
    @records.setPuzzleId(puzzle.get("id"))
    @records.fetch(data: {user_id: Cubemania.currentUser.get("id")})

  checkForNewRecord: (e, request, options) =>
    if request.getResponseHeader("X-NewRecord")
      @records.on("reset", @newRecordsArrived, this)
      @records.fetch(data: $.param(user_id: Cubemania.currentUser.get("id")))

  newRecordsArrived: (records) ->
    r = new Cubemania.Presenters.RecordsPresenter(records)
    Cubemania.flashView.show r.flashMessage(@collection.lastSingle())
    @records.off("reset", @newRecordsArrived)

  onDispose: ->
    $(document).unbind("ajaxComplete")

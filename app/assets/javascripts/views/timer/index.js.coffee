class Cubemania.Views.TimerIndex extends Cubemania.BaseView
  template: JST["timer/index"]

  initialize: (options) ->
    @records = options.records
    @statsView = @addSubview new Cubemania.Views.Stats(singles: @collection, records: @records)
    @timerView = @addSubview new Cubemania.Views.Timer(collection: @collection)
    @chartView = @addSubview new Cubemania.Views.Chart(collection: @collection)
    @singlesView = @addSubview new Cubemania.Views.Singles(collection: @collection)
    $(document).ajaxComplete(@checkForNewRecord)
    @refetchRecordsIntervalId = setInterval(@refetchRecords, 600000) # refetch records every 10 minutes
    @bindTo @collection, "destroy", @displayRecordBackgroundJobHint, this

  render: ->
    $(@el).html(@template())

    @timerView.setElement(@$("#timer")).render()
    @statsView.setElement(@$("#stats")).render()
    @chartView.setElement(@$("#chart-container")).render()
    @singlesView.setElement(@$("#singles")).render()

    this

  checkForNewRecord: (e, request, options) =>
    if request.getResponseHeader("X-NewRecord")
      @records.on("reset", @newRecordsArrived, this)
      @records.fetch(data: $.param(user_id: Cubemania.currentUser.get("id")))

  refetchRecords: =>
    @records.setPuzzleId(Cubemania.currentPuzzle.puzzle.get("id"))
    @records.fetch(data: {user_id: Cubemania.currentUser.get("id")})

  newRecordsArrived: (records) ->
    r = new Cubemania.Presenters.RecordsPresenter(records)
    Cubemania.flashView.slideDown r.flashMessage(@collection.lastSingle())
    @records.off("reset", @newRecordsArrived)

  displayRecordBackgroundJobHint: (single) ->
    Cubemania.flashView.slideDown "Your records are currently being recalculated. This might take up to <strong>ten minutes</strong>."

  onDispose: ->
    $(document).unbind("ajaxComplete")
    clearInterval(@refetchRecordsIntervalId)

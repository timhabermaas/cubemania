class Cubemania.Views.TimerIndex extends Backbone.View
  template: JST["timer/index"]

  initialize: (options) ->
    @records = options.records
    @statsView = new Cubemania.Views.Stats(singles: @collection, records: @records)
    @timerView = new Cubemania.Views.Timer(collection: @collection)
    if Cubemania.currentUser.present()
      @chartView = new Cubemania.Views.Chart(collection: @collection)

    @singlesView = new Cubemania.Views.Singles(collection: @collection)
    $(document).ajaxComplete(@checkForNewRecord)
    @refetchRecordsIntervalId = setInterval(@refetchRecords, 600000) # refetch records every 10 minutes

  render: ->
    $(@el).html(@template())

    @timerView.setElement(@$("#timer")).render()
    @statsView.setElement(@$("#stats")).render()
    if (@chartView?)
      @chartView.setElement(@$("#chart-container")).render()
    else
      @$("#chart-container").html("<p class='suggestion'>You're currently not logged in!<br /> <a href='/login'>Login</a> or <a href='/register'>register</a> to save your times permanently. </p>")
    @singlesView.setElement(@$("#singles")).render()

    this

  checkForNewRecord: (e, request, options) =>
    if request.getResponseHeader("X-NewRecord")
      @records.on "reset", @newRecordsArrived, this
      @records.fetch(data: $.param(user_id: Cubemania.currentUser.get("id")))

  refetchRecords: =>
    @records.setPuzzleId(Cubemania.currentPuzzle.getId())
    @records.fetch(data: {user_id: Cubemania.currentUser.get("id")})

  newRecordsArrived: (records) ->
    r = new Cubemania.Presenters.RecordsPresenter(records)
    Cubemania.flashView.slideDown r.flashMessage(@collection.lastSingle())
    @records.off("reset", @newRecordsArrived)

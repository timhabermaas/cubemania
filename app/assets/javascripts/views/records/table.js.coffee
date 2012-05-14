class Cubemania.Views.RecordsTable extends Cubemania.BaseView # TODO change name. It contains more than the table

  template: JST["records/table"]

  initialize: ->
    @bindTo @collection, "reset", @render, this
    @bindTo @collection, "remove", @render, this
    @bindTo @collection, "add", @recordAdded, this
    @displayChallenge = false

  render: (records) ->
    $(@el).html(@template(records: @collection, timerPath: "/puzzles/#{Cubemania.currentPuzzle.puzzle.get("slug")}/timer", displayChallenge: @displayChallenge))
    _.each(@collection.models, @appendRecord)
    @displayChallenge = true # TODO add flag to collection ("has been synced with server")
    this

  appendRecord: (record, index) ->
    view = new Cubemania.Views.Record(model: record, index: index)
    @$("#records tbody").append(view.render().el)

  recordAdded: (record, records, options) ->
    @appendRecord(record, options.index)

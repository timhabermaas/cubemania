class Cubemania.Views.RecordsIndex extends Cubemania.BaseView

  template: JST["records/index"]

  events:
    "click .tabs .single": "clickSingle"
    "click .tabs .avg5": "clickAvg5"
    "click .tabs .avg12": "clickAvg12"

  initialize: ->
    @recordsTable = @addSubview new Cubemania.Views.RecordsTable(collection: @collection)
    @bindTo Cubemania.currentPuzzle, "change", @refetchRecords, this

  render: ->
    $(@el).html(@template())
    $(@el).append(@recordsTable.el)
    this

  clickSingle: (event) ->
    event.preventDefault()
    @collection.fetch(data: $.param(type: "single"))
    @$(".tabs a").removeClass("selected")
    @$(".tabs a.single").addClass("selected")

  clickAvg5: (event) ->
    event.preventDefault()
    @collection.fetch(data: $.param(type: "avg5"))
    @$(".tabs a").removeClass("selected")
    @$(".tabs a.avg5").addClass("selected")

  clickAvg12: (event) ->
    event.preventDefault()
    @collection.fetch(data: $.param(type: "avg12"))
    @$(".tabs a").removeClass("selected")
    @$(".tabs a.avg12").addClass("selected")

  refetchRecords: (puzzle) ->
    @collection.setPuzzleId puzzle.get("id")
    @collection.fetch()

class Cubemania.Views.RecordsIndex extends Backbone.View

  template: JST["records/index"]

  events:
    "click .tabs .single": "clickSingle"
    "click .tabs .avg5": "clickAvg5"
    "click .tabs .avg12": "clickAvg12"

  initialize: ->
    @recordsTable = new Cubemania.Views.RecordsTable(collection: @collection)

  render: ->
    $(@el).html(@template())
    $(@el).append(@recordsTable.el)
    this

  clickSingle: (event) ->
    event.preventDefault()
    @collection.type = "single"
    @collection.fetch()
    $(".tabs a").removeClass("selected")
    $(".tabs a.single").addClass("selected")

  clickAvg5: (event) ->
    event.preventDefault()
    @collection.type = "avg5"
    @collection.fetch()
    $(".tabs a").removeClass("selected")
    $(".tabs a.avg5").addClass("selected")

  clickAvg12: (event) ->
    event.preventDefault()
    @collection.type = "avg12"
    @collection.fetch()
    $(".tabs a").removeClass("selected")
    $(".tabs a.avg12").addClass("selected")

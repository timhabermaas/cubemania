class Cubemania.Views.RecordsIndex extends Backbone.View

  template: JST["records/index"]

  events:
    "click .tabs .single": "clickSingle"
    "click .tabs .avg5": "clickAvg5"
    "click .tabs .avg12": "clickAvg12"

  initialize: ->
    @collection.on("reset", @initTableView, this)
    @collection.on("reset", @renderTable, this)

  initTableView: ->
    @tableView = new Cubemania.Views.RecordsTable(collection: @collection)

  render: ->
    $(@el).html(@template(records: @collection))
    @renderTable()
    this

  renderTable: ->
    @$("#records").html(@tableView.render().el) if @tableView
    this

  clickSingle: ->
    @collection.type = "single"
    @collection.fetch()
    $(".tabs a").removeClass("selected")
    $(".tabs a.single").addClass("selected")

  clickAvg5: ->
    @collection.type = "avg5"
    @collection.fetch()
    $(".tabs a").removeClass("selected")
    $(".tabs a.avg5").addClass("selected")

  clickAvg12: ->
    @collection.type = "avg12"
    @collection.fetch()
    $(".tabs a").removeClass("selected")
    $(".tabs a.avg12").addClass("selected")

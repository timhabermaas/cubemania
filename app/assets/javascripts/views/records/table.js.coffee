class Cubemania.Views.RecordsTable extends Backbone.View

  template: JST["records/table"]

  tagName: "table"
  id: "records"

  initialize: ->
    @collection.on("reset", @render, this)

  render: ->
    $(@el).html(@template(records: @collection))
    this

class Cubemania.Views.RecordsTable extends Backbone.View

  template: JST["records/table"]

  initialize: ->
    @collection.on("reset", @render, this)

  render: ->
    $(@el).html(@template(records: @collection))
    this

class Cubemania.Views.RecordsTable extends Cubemania.BaseView

  template: JST["records/table"]

  tagName: "table"
  id: "records"

  initialize: ->
    @bindTo @collection, "reset", @render, this

  render: ->
    $(@el).html(@template(records: @collection))
    this

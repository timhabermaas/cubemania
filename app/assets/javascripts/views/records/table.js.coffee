class Cubemania.Views.RecordsTable extends Backbone.View

  template: JST["records/table"]

  render: ->
    $(@el).html(@template(records: @collection))
    this

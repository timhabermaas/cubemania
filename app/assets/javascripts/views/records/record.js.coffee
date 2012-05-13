class Cubemania.Views.Record extends Backbone.View
  template: JST["records/record"]

  tagName: "tr"

  initialize: (options) ->
    @index = options.index

  render: ->
    $(@el).html(@template(record: @model, index: @index))
    $(@el).addClass("record rank#{@index + 1}")
    this

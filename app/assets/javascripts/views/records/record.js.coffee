class Cubemania.Views.Record extends Backbone.View
  template: JST["records/record"]

  tagName: "tr"

  events:
    "click a.block": "blockUser"

  initialize: (options) ->
    @index = options.index

  render: ->
    $(@el).html(@template(record: @model, index: @index))
    $(@el).addClass("record rank#{@index + 1}")
    this

  blockUser: (event) ->
    event.preventDefault()
    if confirm("Are you sure?")
      user = new Cubemania.Models.User(id: @model.get("user").id)
      user.block()
      @model.collection.remove(@model)

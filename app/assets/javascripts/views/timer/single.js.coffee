class Cubemania.Views.Single extends Backbone.View
  template: JST["timer/single"]

  tagName: "li"

  events:
    "click .edit .delete": "clickDelete"
    "click .edit .plus2": "clickPlus2"
    "click .edit .dnf": "clickDnf"

  initialize: ->
    @model.bind("change", @render, this);
    @model.bind("destroy", @remove, this);

  render: ->
    $(@el).html(@template(single: @model))
    this

  clickDelete: (event) ->
    event.preventDefault()
    @model.destroy()

  clickPlus2: (event) ->
    event.preventDefault()
    @model.togglePlus2()
    @model.save()

  clickDnf: (event) ->
    event.preventDefault()
    @model.toggleDnf()
    @model.save()

class Cubemania.Views.Single extends Cubemania.BaseView
  template: JST["timer/single"]

  tagName: "li"

  events:
    "click a.delete": "clickDelete"
    "click a.plus2": "clickPlus2"
    "click a.dnf": "clickDnf"

  initialize: ->
    @bindTo @model, "change", @render, this
    @bindTo @model, "destroy", @remove, this

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

class Cubemania.Views.UsersShow extends Backbone.View

  template: JST["users/show"]

  initialize: ->
    @model.on("change", @render, this)

  render: ->
    $(@el).html(@template(user: @model))
    this

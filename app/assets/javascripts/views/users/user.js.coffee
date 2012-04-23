class Cubemania.Views.Users extends Backbone.View
  template: JST["users/users"]

  initialize: ->
    @collection.on("reset", @render, this)

  render: ->
    $(@el).html(@template(users: @collection, maxSingles: @collection.maxSinglesCount()))
    this

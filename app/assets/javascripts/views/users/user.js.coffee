class Cubemania.Views.Users extends Cubemania.BaseView
  template: JST["users/users"]

  initialize: ->
    @bindTo @collection, "reset", @render, this

  render: ->
    $(@el).html(@template(users: @collection, maxSingles: @collection.maxSinglesCount()))
    this

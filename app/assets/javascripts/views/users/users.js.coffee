class Cubemania.Views.Users extends Cubemania.BaseView
  initialize: ->
    @bindTo @collection, "add", @appendUser, this
    @bindTo @collection, "reset", @render, this

  render: ->
    $(@el).html("")
    @maxSinglesCount = @collection.maxSinglesCount()
    _.each(@collection.models, @appendUser)
    this

  appendUser: (user) =>
    userView = new Cubemania.Views.User(model: user, maxSinglesCount: @maxSinglesCount)
    $(@el).append(userView.render().el)

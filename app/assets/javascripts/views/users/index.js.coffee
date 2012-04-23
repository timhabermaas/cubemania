class Cubemania.Views.UsersIndex extends Cubemania.BaseView

  template: JST["users/index"]

  events:
    "submit #users-search": "search"

  initialize: ->
    @usersView = new Cubemania.Views.Users(collection: @collection)

  render: ->
    $(@el).html(@template(users: @collection, max_singles: @collection.maxSinglesCount()))
    @usersView.setElement(@$("#users")).render()
    this

  search: (event) ->
    event.preventDefault()
    @collection.fetch({data: {q: @$("#q").val()}})

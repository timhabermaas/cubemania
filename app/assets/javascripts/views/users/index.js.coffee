class Cubemania.Views.UsersIndex extends Cubemania.BaseView
  template: JST["users/index"]

  events:
    "submit #users-search": "search"
    "click a.load-more": "loadMore"

  initialize: ->
    @usersView = @addSubview new Cubemania.Views.Users(collection: @collection)

  render: ->
    $(@el).html(@template(users: @collection, max_singles: @collection.maxSinglesCount()))
    @usersView.setElement(@$("#users")).render()
    this

  search: (event) ->
    event.preventDefault()
    @collection.search(@$("#q").val())

  loadMore: (event) -> # TODO replace link with spinning ball
    event.preventDefault()
    @collection.loadMore()

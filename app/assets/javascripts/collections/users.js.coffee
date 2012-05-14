class Cubemania.Collections.Users extends Backbone.Collection
  model: Cubemania.Models.User
  url: ->
    if @q?
      "/users?page=#{@currentPage()}&q=#{@q}"
    else
      "/users?page=#{@currentPage()}"

  initialize: (models, options = {}) ->
    @page = options.page
    @q = options.q

  maxSinglesCount: ->
    user = _.max(this.models, (u) -> u.get("singles_count"))
    if user
      user.get("singles_count")
    else
      0

  currentPage: ->
    @page || 1

  loadMore: ->
    @page = @currentPage() + 1
    users = new Cubemania.Collections.Users([], {page: @page, q: @q})
    users.on "reset", @moreUsersArrived, this
    users.fetch()

  search: (q) ->
    @q = q
    @page = 1
    @fetch()

  moreUsersArrived: (users) =>
    @add(users.models)

class Cubemania.Collections.Records extends Backbone.Collection
  model: Cubemania.Models.Record

  url: ->
    "/puzzles/" + @puzzleId + "/records?page=#{@currentPage()}"

  initialize: (models, options) ->
    if options?
      @puzzleId = options.puzzleId
      @page = options.page
      @localStorage = new Backbone.LocalStorage("records-#{options.puzzleId}") if options.useLocalStorage

  setPuzzleId: (puzzleId) ->
    @puzzleId = puzzleId

  getSingleRecord: ->
    @getByAmount(1)

  getAvg5Record: ->
    @getByAmount(5)

  getAvg12Record: ->
    @getByAmount(12)

  currentPage: ->
    @page || 1

  loadMore: -> # TODO extract common behavior into PaginatedCollection class
    @page = @currentPage() + 1
    records = new Cubemania.Collections.Records([], {page: @page, puzzleId: @puzzleId})
    records.on "reset", @moreRecordsArrived, this
    records.fetch()

  moreRecordsArrived: (records) =>
    @add(records.models)

  getByAmount: (amount) ->
    _.find(@models, (r) -> r.get("amount") == amount)

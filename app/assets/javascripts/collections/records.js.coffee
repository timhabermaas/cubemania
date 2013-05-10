class Cubemania.Collections.Records extends Backbone.Collection
  model: Cubemania.Models.Record

  comparator: (record) ->
    record.get("amount")

  url: ->
    "/api/puzzles/" + @puzzleId + "/records/recent?page=#{@currentPage()}&type=#{@getType()}"

  initialize: (models, options) ->
    if options?
      @puzzleId = options.puzzleId
      @type = options.type
      @page = options.page
      @localStorage = new Backbone.LocalStorage("records-#{options.puzzleId}") if options.useLocalStorage

  setPuzzleId: (puzzleId) ->
    @puzzleId = puzzleId

  setType: (type) ->
    @type = type

  getSingleRecord: ->
    @getByAmount(1)

  getAvg5Record: ->
    @getByAmount(5)

  getAvg12Record: ->
    @getByAmount(12)

  currentPage: ->
    @page || 1

  getType: ->
    @type || "avg5"

  loadMore: -> # TODO extract common behavior into PaginatedCollection class
    @page = @currentPage() + 1
    records = new Cubemania.Collections.Records([], {page: @page, puzzleId: @puzzleId, type: @getType()})
    records.on "reset", @moreRecordsArrived, this
    records.fetch()

  moreRecordsArrived: (records) =>
    @add(records.models)

  getByAmount: (amount) ->
    _.find(@models, (r) -> r.get("amount") == amount)

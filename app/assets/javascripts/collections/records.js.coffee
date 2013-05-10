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

  currentPage: ->
    @page || 1

  getType: ->
    @type || "avg5"

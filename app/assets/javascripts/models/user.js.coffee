class Cubemania.Models.User extends Backbone.Model

  urlRoot: "/users"

  initialize: ->
    this.records = new Cubemania.Collections.Records()
    @bind("change", @setupRecords, this)

  setupRecords: ->
    @records = new Cubemania.Collections.Records(@get("records") || [])

  activity: (max) ->
    if max == 0
      1
    else
      @get("singles_count") / max

class Cubemania.Models.User extends Backbone.Model

  urlRoot: "/api/users"

  initialize: ->
    this.records = new Cubemania.Collections.Records()
    @bind("change", @setupRecords, this)

  setupRecords: ->
    @records = new Cubemania.Collections.Records(@get("records") || [])

  present: ->
    @get("id")?

  activity: (max) -> # TODO get max from collection
    if max == 0
      1
    else
      @get("singles_count") / max

  isAdmin: ->
    @get("role") == "admin"

  isModerator: ->
    @get("role") == "moderator"

  isAdminOrModerator: ->
    @isAdmin() || @isModerator()

  block: ->
    (@sync || Backbone.sync).call this, "block", this,
      url: @url() + "/block"
      type: "POST"
      data: null

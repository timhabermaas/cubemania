class Cubemania.Models.User extends Backbone.Model

  urlRoot: "/users"

  initialize: ->
    this.records = new Cubemania.Collections.Records()
    @bind("change", @setupRecords, this)

  setupRecords: ->
    @records = new Cubemania.Collections.Records(@get("records") || [])

  present: ->
    @get("id")?

  activity: (max) ->
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
    console.log this.url()
    (@sync || Backbone.sync).call this, "block", this,
      url: @url() + "/block"
      type: "POST"
      data: null

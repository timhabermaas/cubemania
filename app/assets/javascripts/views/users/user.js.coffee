class Cubemania.Views.User extends Cubemania.BaseView
  template: JST["users/user"]

  tagName: "li"

  initialize: (options) ->
    @maxSinglesCount = options.maxSinglesCount

  render: ->
    $(@el).html(@template(user: @model, maxSinglesCount: @maxSinglesCount))
    $(@el).css("font-size", "#{@model.activity(@maxSinglesCount) * 1.4 + 0.6}em")
    this

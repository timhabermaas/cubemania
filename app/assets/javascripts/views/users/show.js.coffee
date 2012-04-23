class Cubemania.Views.UsersShow extends Cubemania.BaseView

  template: JST["users/show"]
  id: "user"

  initialize: ->
    @bindTo @model, "change", @render, this

  wcaLink: ->
    '<a href="http://www.worldcubeassociation.org/results/p.php?i=' + @model.get('wca') + '">' + @model.get("name") + '\'s World Cube Association Profile</a>'

  render: ->
    $(@el).html(@template(user: @model, wcaLink: @wcaLink(), records: @model.records.models))
    this

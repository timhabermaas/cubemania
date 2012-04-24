class Cubemania.Views.Navigation extends Backbone.View

  events:
    "click a": "clickItem"

  clickItem: (event) ->
    ul = @$("ul").children("li").removeClass("selected")
    $(event.currentTarget).parent().addClass("selected")

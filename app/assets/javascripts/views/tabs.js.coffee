class Cubemania.Views.Tabs extends Backbone.View
  tagName: "p"
  className: "tabs"

  events:
    "click a": "tabClicked"

  tabClicked: (event) =>
    event.preventDefault()
    clickedTab =_.find @tabs, (t) -> t.className == event.currentTarget.className
    @$("a").removeClass("selected")
    @$("a.#{clickedTab.className}").addClass("selected")
    clickedTab.callback() if clickedTab.callback

  initialize: (title, tabs, selectedIndex) ->
    @title = title
    @tabs = tabs
    @selectedIndex = selectedIndex

  link: (tab, selected) ->
    "<a href='#' class='#{tab.className}" + (if selected then " selected" else "") + "'>#{tab.name}</a>"

  render: ->
    el = $(@el)
    $(@el).html("")
    links = (@link(tab, index == @selectedIndex) for tab, index in @tabs)
    el.append @title + " " + links.join(" ")
    @el

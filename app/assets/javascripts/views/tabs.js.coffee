class Cubemania.Views.Tabs extends Cubemania.BaseView
  tagName: "p"
  className: "tabs"

  events:
    "click a": "tabClicked"

  tabClicked: (event) =>
    event.preventDefault()
    target = event.currentTarget
    unless $(target).hasClass("selected")
      clickedTab =_.find @tabs, (t) -> t.className == target.className
      @$("a").removeClass("selected")
      @$("a.#{clickedTab.className}").addClass("selected")
      clickedTab.callback() if clickedTab.callback

  initialize: (options) ->
    @title = options.title
    @tabs = options.tabs
    @selectedIndex = options.selectedIndex

  link: (tab, selected) ->
    "<a href='#' class='#{tab.className}" + (if selected then " selected" else "") + "'>#{tab.name}</a>"

  render: ->
    el = $(@el)
    $(@el).html("")
    links = (@link(tab, index == @selectedIndex) for tab, index in @tabs)
    el.append @title + " " + links.join(" ")
    this

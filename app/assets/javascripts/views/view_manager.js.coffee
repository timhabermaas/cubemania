class Cubemania.ViewManager
  constructor: ->
    @currentView = null

  changeView: (view) ->
    @currentView.dispose() if @currentView?
    @currentView = view

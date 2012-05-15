class Cubemania.ViewManager
  constructor: ->
    @currentView = null

  newView: (klass, options) ->
    @currentView.dispose() if @currentView?
    @currentView = new klass(options)

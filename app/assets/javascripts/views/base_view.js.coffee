class Cubemania.BaseView extends Backbone.View
  constructor: (options) ->
    @bindings = []
    @subviews = []
    Backbone.View.apply(this, [options])

  bindTo: (object, event, callback, context) ->
    object.on event, callback, context
    @bindings.push {object: object, event: event, callback: callback, context: context}

  addSubview: (view) ->
    @subviews.push view
    view

  unbindFromAll: ->
    b.object.off b.event, b.callback, b.context for b in @bindings
    @bindings = []

  onDispose: ->

  dispose: ->
    view.dispose() for view in @subviews
    @onDispose()
    @unbindFromAll() # remove all manual bindings from view
    @unbind() # unbind all events defined through "events" property
    @remove() # remove element from DOM

Cubemania.BaseView.extend = Backbone.View.extend

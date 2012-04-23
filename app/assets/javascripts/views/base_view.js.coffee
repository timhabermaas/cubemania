class Cubemania.BaseView extends Backbone.View
  constructor: (options) ->
    @bindings = []
    Backbone.View.apply(this, [options])

  bindTo: (object, event, callback, context) ->
    object.on event, callback, context
    @bindings.push {object: object, event: event, callback: callback, context: context}

  unbindFromAll: ->
    _.each(@bindings, (b) ->
      b.object.off b.event, b.callback, b.context
    )
    @bindings = []

  dispose: ->
    @unbindFromAll()
    @unbind()
    @remove()

Cubemania.BaseView.extend = Backbone.View.extend

class Cubemania.BaseView
  constructor: (options) ->
    this.bindings = []
    Backbone.View.apply(this, [options])

_.extend Cubemania.BaseView.prototype, Backbone.View.prototype,
  bindTo: (object, event, callback, context) ->
    object.on event, callback, context
    @bindings.push {object: object, event: event, callback: callback, context: context}

Cubemania.BaseView.extend = Backbone.View.extend

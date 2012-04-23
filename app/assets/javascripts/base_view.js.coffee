class Cubemania.BaseView
  constructor: (options) ->
    this.bindings = []
    Backbone.View.apply(this, [options])

_.extend Cubemania.BaseView.prototype, Backbone.View.prototype,
  bindTo: (object, event, callback, scope) ->
    object.on event, callback, scope
    @bindings.push {object: object, event: event, callback: callback}

Cubemania.BaseView.extend = Backbone.View.extend

// Generated by CoffeeScript 1.10.0
jQuery(function() {
  if (!$("#backbone-container").length) {
    return;
  }

  (function() {
    var bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
      extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
      hasProp = {}.hasOwnProperty;

    Cubemania.Views.Singles = (function(superClass) {
      extend(Singles, superClass);

      function Singles() {
        this.appendSingle = bind(this.appendSingle, this);
        this.prependSingle = bind(this.prependSingle, this);
        return Singles.__super__.constructor.apply(this, arguments);
      }

      Singles.prototype.template = _.template($("#template-timer-singles").html());

      Singles.prototype.events = {
        "click .load-more a": "loadMore"
      };

      Singles.prototype.initialize = function() {
        this.collection.on("reset", this.onReset, this);
        this.collection.on("add", this.prependSingle, this);
        this.collection.on("add", this.increaseIndex, this);
        this.collection.on("remove", this.decreaseIndex, this);
        this.collection.on("reset", this.hideOrShowSuggestion, this);
        this.collection.on("add", this.hideOrShowSuggestion, this);
        this.collection.on("remove", this.hideOrShowSuggestion, this);
        return this.nextSingleIndex = 0;
      };

      Singles.prototype.onReset = function() {
        this.nextSingleIndex = this.collection.today().length;
        return this.render();
      };

      Singles.prototype.render = function() {
        $(this.el).html(this.template({
          singles: this.collection
        }));
        _.each(this.recentSingles(), this.prependSingle);
        return this;
      };

      Singles.prototype.prependSingle = function(single) {
        var view;
        view = new Cubemania.Views.Single({
          model: single
        });
        return this.$("ol").prepend(view.render().el);
      };

      Singles.prototype.appendSingle = function(single) {
        var view;
        view = new Cubemania.Views.Single({
          model: single
        });
        return this.$("ol").append(view.render().el);
      };

      Singles.prototype.loadMore = function(event) {
        var singles;
        event.preventDefault();
        singles = this.collection.models.slice(-(this.nextSingleIndex + 12), +(-this.nextSingleIndex - 1) + 1 || 9e9);
        _.each(singles.reverse(), this.appendSingle);
        this.nextSingleIndex += 12;
        return this.hideOrShowSuggestion();
      };

      Singles.prototype.increaseIndex = function() {
        return this.nextSingleIndex += 1;
      };

      Singles.prototype.decreaseIndex = function() {
        return this.nextSingleIndex -= 1;
      };

      Singles.prototype.recentSingles = function() {
        if (this.nextSingleIndex === 0) {
          return [];
        } else {
          return this.collection.models.slice(-this.nextSingleIndex);
        }
      };

      Singles.prototype.hideOrShowSuggestion = function() {
        if (this.nextSingleIndex === 0) {
          return this.$("p.suggestion").show();
        } else {
          return this.$("p.suggestion").hide();
        }
      };

      return Singles;

    })(Backbone.View);

  }).call(this);
});
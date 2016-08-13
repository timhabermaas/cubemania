// Generated by CoffeeScript 1.10.0
(function() {
  var extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
    hasProp = {}.hasOwnProperty;

  Cubemania.Models.Puzzle = (function(superClass) {
    extend(Puzzle, superClass);

    function Puzzle() {
      return Puzzle.__super__.constructor.apply(this, arguments);
    }

    Puzzle.prototype.initialize = function() {
      return this.kind = new Cubemania.Models.Kind(this.get("kind"));
    };

    Puzzle.prototype.getId = function() {
      return this.get("id");
    };

    Puzzle.prototype.getName = function() {
      return this.get("name");
    };

    Puzzle.prototype.getFullName = function() {
      return this.get("name") + " " + this.kind.get("name");
    };

    return Puzzle;

  })(Backbone.Model);

}).call(this);
// Generated by CoffeeScript 1.10.0
jQuery(function() {
  if (!$("#backbone-container").length) {
    return;
  }

  (function() {
    var extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
      hasProp = {}.hasOwnProperty;

    Cubemania.Views.Single = (function(superClass) {
      extend(Single, superClass);

      function Single() {
        return Single.__super__.constructor.apply(this, arguments);
      }

      Single.prototype.template = _.template($("#template-timer-single").html());

      Single.prototype.tagName = "li";

      Single.prototype.events = {
        "click a.delete": "clickDelete",
        "click a.plus2": "clickPlus2",
        "click a.dnf": "clickDnf"
      };

      Single.prototype.initialize = function() {
        this.model.on("change", this.render, this);
        return this.model.on("destroy", this.remove, this);
      };

      Single.prototype.render = function() {
        $(this.el).html(this.template({
          single: this.model
        }));
        return this;
      };

      Single.prototype.clickDelete = function(event) {
        event.preventDefault();
        this.displayRecordBackgroundJobHint();
        return this.model.destroy();
      };

      Single.prototype.clickPlus2 = function(event) {
        event.preventDefault();
        this.displayRecordBackgroundJobHint();
        this.model.togglePlus2();
        return this.model.save();
      };

      Single.prototype.clickDnf = function(event) {
        event.preventDefault();
        this.displayRecordBackgroundJobHint();
        this.model.toggleDnf();
        return this.model.save();
      };

      Single.prototype.displayRecordBackgroundJobHint = function(single) {
        if (Cubemania.currentUser.present()) {
          return Cubemania.flashView.slideDown("Your records are currently being recalculated. This might take up to <strong>ten minutes</strong>.");
        }
      };

      return Single;

    })(Backbone.View);

  }).call(this);
});
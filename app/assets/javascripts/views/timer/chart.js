// Generated by CoffeeScript 1.10.0
jQuery(function() {
  if (!$("#backbone-container").length) {
    return;
  }

  (function() {
    var extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
      hasProp = {}.hasOwnProperty;

    Cubemania.Views.Chart = (function(superClass) {
      extend(Chart, superClass);

      function Chart() {
        return Chart.__super__.constructor.apply(this, arguments);
      }

      Chart.prototype.template = _.template($("#template-timer-chart").html());

      Chart.prototype.events = {
        "click a.by-solve": "bySolve",
        "click a.by-date": "byDate"
      };

      Chart.COLORS = ['rgba(81, 115, 151, 0.9)', 'rgba(230, 138, 23, 0.9)', 'rgba(141, 16, 150, 0.9)', 'rgba(192, 223, 22, 0.9)'];

      Chart.prototype.byDate = function(event) {
        event.preventDefault();
        this.$("p.tabs a").removeClass("selected");
        $(event.currentTarget).addClass("selected");
        if (this.singleChart) {
          this.createDateChart();
          this.addCurrentUserToChart();
          this.singleChart = false;
          this.$("p.help").show();
          return this.$("ul.token-input-list-facebook").show();
        }
      };

      Chart.prototype.bySolve = function(event) {
        event.preventDefault();
        this.$("p.tabs a").removeClass("selected");
        $(event.currentTarget).addClass("selected");
        if (!this.singleChart) {
          this.createSinglesChart();
          this.singleChart = true;
          this.$("p.help").hide();
          return this.$("ul.token-input-list-facebook").hide();
        }
      };

      Chart.prototype.initialize = function() {
        this.collection.on("reset", this.render, this);
        this.collection.on("reset", this.createSinglesChart, this);
        this.collection.on("remove", this.removeSingleFromChart, this);
        this.collection.on("change", this.updateSingleInChart, this);
        return this.singleChart = true;
      };

      Chart.prototype.createSinglesChart = function() {
        var data;
        data = _.map(this.collection.models, this.dataPointFromSingle);
        return this.chart = new Highcharts.Chart({
          chart: {
            renderTo: this.$("#chart")[0],
            type: "scatter"
          },
          title: {
            text: Cubemania.currentPuzzle.getFullName()
          },
          tooltip: {
            formatter: function() {
              var t;
              t = (formatDateTime(this.point.date)) + "<br/>Time: <b>" + (formatTime(this.y)) + "</b>";
              if (this.point.comment != null) {
                t += "<br/><i>" + (formatScramble(this.point.comment)) + "</i>";
              }
              return t;
            }
          },
          plotOptions: {
            scatter: {
              marker: {
                radius: 7,
                symbol: "circle",
                states: {
                  hover: {
                    enabled: true,
                    lineColor: 'rgb(100,100,100)'
                  }
                }
              },
              states: {
                hover: {
                  marker: {
                    enabled: false
                  }
                }
              }
            }
          },
          yAxis: {
            title: {
              text: "Time"
            },
            labels: {
              formatter: function() {
                return formatTime(this.value);
              }
            }
          },
          series: [
            {
              name: Cubemania.currentUser.get("name"),
              data: data,
              color: Cubemania.Views.Chart.COLORS[0]
            }
          ]
        });
      };

      Chart.prototype.dataPointFromSingle = function(single) {
        return {
          id: single.get("id"),
          y: single.get("time"),
          date: single.get("created_at"),
          comment: single.get("comment")
        };
      };

      Chart.prototype.addSingleToChart = function(single) {
        if (!this.singleChart) {
          return;
        }
        return this.chart.series[0].addPoint(this.dataPointFromSingle(single));
      };

      Chart.prototype.removeSingleFromChart = function(single) {
        if (!this.singleChart) {
          return;
        }
        return this.chart.get(single.get("id")).remove();
      };

      Chart.prototype.updateSingleInChart = function(single) {
        var point;
        if (!this.singleChart) {
          return;
        }
        point = this.chart.get(single.get("id"));
        if (point != null) {
          return point.update(this.dataPointFromSingle(single));
        } else {
          return this.addSingleToChart(single);
        }
      };

      Chart.prototype.createDateChart = function() {
        return this.chart = new Highcharts.Chart({
          chart: {
            renderTo: this.$("#chart")[0],
            type: "scatter",
            zoomType: "x"
          },
          title: {
            text: "Your cubing progress in " + (Cubemania.currentPuzzle.getFullName())
          },
          subtitle: {
            text: this.subtitle()
          },
          tooltip: {
            formatter: function() {
              return (formatDate(this.x)) + "<br/>Mean: <b>" + (formatTime(this.y)) + "</b>";
            }
          },
          plotOptions: {
            scatter: {
              marker: {
                radius: 7,
                symbol: "circle",
                states: {
                  hover: {
                    enabled: true,
                    lineColor: 'rgb(100,100,100)'
                  }
                }
              },
              states: {
                hover: {
                  marker: {
                    enabled: false
                  }
                }
              }
            }
          },
          xAxis: {
            type: "datetime",
            events: {
              setExtremes: (function(_this) {
                return function(event) {
                  var userIds;
                  userIds = _this.userIdsInChart();
                  if (event.min != null) {
                    return _this.updateDataForUsers(userIds, event.min / 1000, event.max / 1000);
                  } else {
                    return _this.updateDataForUsers(userIds, null, null);
                  }
                };
              })(this)
            }
          },
          yAxis: {
            title: {
              text: "Time"
            },
            labels: {
              formatter: function() {
                return formatTime(this.value);
              }
            }
          },
          series: []
        });
      };

      Chart.prototype.render = function() {
        $(this.el).html(this.template());
        this.$("p.help").hide();
        this.$("#user-tokens").tokenInput("/api/users", {
          crossDomain: false,
          theme: "facebook",
          preventDuplicates: true,
          onResult: function(results) {
            return _.filter(results, function(r) {
              return r.id !== Cubemania.currentUser.get("id");
            });
          },
          hintText: "Compare with...",
          onAdd: (function(_this) {
            return function(item) {
              return _this.addUserToChart(item.id, item.name);
            };
          })(this),
          onDelete: (function(_this) {
            return function(item) {
              return _this.removeUserFromChart(item.id);
            };
          })(this)
        });
        this.$("ul.token-input-list-facebook").hide();
        return this;
      };

      Chart.prototype.subtitle = function(data) {
        var from, to;
        if (data == null) {
          data = [];
        }
        if (data.length > 0) {
          from = formatDate(new Date(data[0].x));
          to = formatDate(new Date(data[data.length - 1].x));
          return "from " + from + " to " + to;
        } else {
          return "from ? to ?";
        }
      };

      Chart.prototype.addCurrentUserToChart = function() {
        return this.addUserToChart(Cubemania.currentUser.get("id"), Cubemania.currentUser.get("name"));
      };

      Chart.prototype.fetchDataForChart = function(userId, from, to, callback) {
        var puzzleId;
        puzzleId = Cubemania.currentPuzzle.getId();
        var fromParam = "";
        var toParam = "";
        if (from) {
          fromParam += "from=" + from + "&"
        }
        if (to) {
          toParam += "to=" + to + "&"
        }
        return $.getJSON("/api/puzzles/" + puzzleId + "/singles/chart.json?" + fromParam + toParam + "&user_id=" + userId, (function(_this) {
          return function(data) {
            return callback(_this.generateChartDataFromApiData(data));
          };
        })(this));
      };

      Chart.prototype.addUserToChart = function(id, name) {
        var color, count;
        count = this.chart.series.length;
        color = Cubemania.Views.Chart.COLORS[count % Cubemania.Views.Chart.COLORS.length];
        return this.fetchDataForChart(id, null, null, (function(_this) {
          return function(data) {
            _this.chart.addSeries({
              id: id,
              name: name,
              color: color,
              data: data
            });
            return _this.chart.setTitle({}, {
              text: _this.subtitle(data)
            });
          };
        })(this));
      };

      Chart.prototype.updateDataForUser = function(id, from, to) {
        return this.fetchDataForChart(id, from, to, (function(_this) {
          return function(data) {
            _this.chart.get(id).setData(data);
            return _this.chart.setTitle({}, {
              text: _this.subtitle(data)
            });
          };
        })(this));
      };

      Chart.prototype.updateDataForUsers = function(ids, from, to) {
        var i, id, len, results1;
        results1 = [];
        for (i = 0, len = ids.length; i < len; i++) {
          id = ids[i];
          results1.push(this.updateDataForUser(id, from, to));
        }
        return results1;
      };

      Chart.prototype.generateChartDataFromApiData = function(apiData) {
        return _.map(apiData, function(single) {
          return {
            x: single.created_at_timestamp * 1000,
            y: single.time
          };
        });
      };

      Chart.prototype.userIdsInChart = function() {
        var ids;
        ids = _.pluck(this.$("#user-tokens").tokenInput("get"), "id");
        return ids.concat([Cubemania.currentUser.get("id")]);
      };

      Chart.prototype.removeUserFromChart = function(id) {
        return this.chart.get(id).remove();
      };

      return Chart;

    })(Backbone.View);

  }).call(this);
});

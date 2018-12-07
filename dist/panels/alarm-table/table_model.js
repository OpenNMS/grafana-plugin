'use strict';

System.register(['../../opennms'], function (_export, _context) {
  "use strict";

  var Model, _createClass, TableModel;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [function (_opennms) {
      Model = _opennms.Model;
    }],
    execute: function () {
      _createClass = function () {
        function defineProperties(target, props) {
          for (var i = 0; i < props.length; i++) {
            var descriptor = props[i];
            descriptor.enumerable = descriptor.enumerable || false;
            descriptor.configurable = true;
            if ("value" in descriptor) descriptor.writable = true;
            Object.defineProperty(target, descriptor.key, descriptor);
          }
        }

        return function (Constructor, protoProps, staticProps) {
          if (protoProps) defineProperties(Constructor.prototype, protoProps);
          if (staticProps) defineProperties(Constructor, staticProps);
          return Constructor;
        };
      }();

      _export('TableModel', TableModel = function () {
        function TableModel() {
          _classCallCheck(this, TableModel);

          this.columns = [];
          this.rows = [];
          this.type = 'table';
        }

        _createClass(TableModel, [{
          key: 'severityForLabel',
          value: function severityForLabel(label) {
            var sev = Model.Severities[label];
            if (sev) {
              return sev.id;
            } else {
              console.warn('Unable to determine severity for "' + label + '".');
              return -1;
            }
          }
        }, {
          key: 'sort',
          value: function sort(options) {
            if (options.col === null || this.columns.length <= options.col) {
              return;
            }

            var self = this;
            this.rows.sort(function (a, b) {
              var colInfo = self.columns[options.col];

              // by default just use the column as-is (a string)
              a = '' + a[options.col];
              b = '' + b[options.col];

              if (colInfo && colInfo.style) {
                var type = colInfo.style.type;

                if (type === 'number') {
                  // if it's a number type, cast it
                  a = Number(a);
                  b = Number(b);
                } else if (type === 'severity') {
                  // if it's a severity, get the numeric value
                  a = self.severityForLabel(a);
                  b = self.severityForLabel(b);
                }
              }

              if (a < b) {
                return -1;
              }
              if (a > b) {
                return 1;
              }
              return 0;
            });

            this.columns.forEach(function (col) {
              return col.sort = false;
            });
            this.columns[options.col].sort = true;

            if (options.desc) {
              this.rows.reverse();
              this.columns[options.col].desc = true;
            } else {
              this.columns[options.col].desc = false;
            }
          }
        }]);

        return TableModel;
      }());

      _export('TableModel', TableModel);
    }
  };
});
//# sourceMappingURL=table_model.js.map

'use strict';

System.register([], function (_export, _context) {
  "use strict";

  var _createClass, TableModel;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [],
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
          key: 'sort',
          value: function sort(options) {
            if (options.col === null || this.columns.length <= options.col) {
              return;
            }

            this.rows.sort(function (a, b) {
              a = a[options.col];
              b = b[options.col];
              if (a < b) {
                return -1;
              }
              if (a > b) {
                return 1;
              }
              return 0;
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

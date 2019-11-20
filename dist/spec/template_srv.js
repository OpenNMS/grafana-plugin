'use strict';

System.register(['lodash'], function (_export, _context) {
  "use strict";

  var _, _createClass, TemplateSrv;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [function (_lodash) {
      _ = _lodash.default;
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

      _export('TemplateSrv', TemplateSrv = function () {
        function TemplateSrv() {
          _classCallCheck(this, TemplateSrv);

          this.variables = [];
          this.grafanaVariables = {};
          this.index = {};
          /*
           * This regex matches 3 types of variable reference with an optional format specifier
           * \$(\w+)                          $var1
           * \[\[([\s\S]+?)(?::(\w+))?\]\]    [[var2]] or [[var2:fmt2]]
           * \${(\w+)(?::(\w+))?}             ${var3} or ${var3:fmt3}
          */
          this.regex = /\$(\w+)|\[\[([\s\S]+?)(?::(\w+))?\]\]|\${(\w+)(?::(\w+))?}/g;
        }

        _createClass(TemplateSrv, [{
          key: 'init',
          value: function init(variables) {
            this.variables = variables;
            this.updateTemplateData();
          }
        }, {
          key: 'updateTemplateData',
          value: function updateTemplateData() {
            this.index = {};

            for (var i = 0; i < this.variables.length; i++) {
              var variable = this.variables[i];

              if (!variable.current || !variable.current.isNone && !variable.current.value) {
                continue;
              }

              this.index[variable.name] = variable;
            }
          }
        }, {
          key: 'formatValue',
          value: function formatValue(value, format, variable) {
            // for some scopedVars there is no variable
            variable = variable || {};

            if (typeof format === 'function') {
              return format(value, variable, this.formatValue);
            }

            switch (format) {
              case 'regex':
                {
                  throw new Error('not supported');
                }
              case 'lucene':
                {
                  throw new Error('not supported');
                }
              case 'pipe':
                {
                  if (typeof value === 'string') {
                    return value;
                  }
                  return value.join('|');
                }
              case 'distributed':
                {
                  throw new Error('not supported');
                }
              default:
                {
                  if (_.isArray(value)) {
                    return '{' + value.join(',') + '}';
                  }
                  return value;
                }
            }
          }
        }, {
          key: 'getVariableName',
          value: function getVariableName(expression) {
            this.regex.lastIndex = 0;
            var match = this.regex.exec(expression);
            if (!match) {
              return null;
            }
            return match[1] || match[2];
          }
        }, {
          key: 'getAllValue',
          value: function getAllValue(variable) {
            if (variable.allValue) {
              return variable.allValue;
            }
            var values = [];
            for (var i = 1; i < variable.options.length; i++) {
              values.push(variable.options[i].value);
            }
            return values;
          }
        }, {
          key: 'replace',
          value: function replace(target, scopedVars, format) {
            var _this = this;

            if (!target) {
              return target;
            }

            var variable, systemValue, value;
            this.regex.lastIndex = 0;

            return target.replace(this.regex, function (match, var1, var2, fmt2, var3, fmt3) {
              variable = _this.index[var1 || var2 || var3];
              format = fmt2 || fmt3 || format;
              if (scopedVars) {
                value = scopedVars[var1 || var2 || var3];
                if (value) {
                  return _this.formatValue(value.value, format, variable);
                }
              }

              if (!variable) {
                return match;
              }

              systemValue = _this.grafanaVariables[variable.current.value];
              if (systemValue) {
                return _this.formatValue(systemValue, format, variable);
              }

              value = variable.current.value;
              if (_this.isAllValue(value)) {
                value = _this.getAllValue(variable);
                // skip formating of custom all values
                if (variable.allValue) {
                  return value;
                }
              }

              var res = _this.formatValue(value, format, variable);
              return res;
            });
          }
        }, {
          key: 'isAllValue',
          value: function isAllValue(value) {
            return value === '$__all' || Array.isArray(value) && value[0] === '$__all';
          }
        }]);

        return TemplateSrv;
      }());

      _export('TemplateSrv', TemplateSrv);

      _export('default', new TemplateSrv());
    }
  };
});
//# sourceMappingURL=template_srv.js.map

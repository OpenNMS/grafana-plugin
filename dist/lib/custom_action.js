'use strict';

System.register(['lodash'], function (_export, _context) {
  "use strict";

  var _, _createClass, failed, escapeRE, CustomAction;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  // eslint-disable-line no-useless-escape

  function makeVariableRE(variableName) {
    var reString = '\\$(' + variableName.replace(escapeRE, '\\$&') + ')\\b(\\[(.*?)\\])?';
    return new RegExp(reString, 'g');
  }

  function getMatch(value, variableName) {
    if (_.isNull(value) || _.isEmpty(value)) {
      return false;
    }

    var match = makeVariableRE(variableName).exec(value);
    if (match) {
      var parsed = parseInt(match[3], 10);
      var ret = {
        token: match[0],
        variable: match[1]
      };
      if (Number.isInteger(parsed)) {
        ret.index = parsed;
        return ret;
      } else if (match[3] !== undefined && match[3].length > 0) {
        ret.index = match[3];
        return ret;
      } else if (match[3] === undefined || match[3] === '') {
        return ret;
      }
    }
    return null;
  }

  _export('getMatch', getMatch);

  function getValue(v) {
    var ret = failed;
    if (v) {
      ret = v;
      if (v.urlValue) {
        // implements IHasUrlValue in OpenNMS.js
        ret = v.urlValue;
      } else {
        // fallback, try to guess at the proper string type
        if (v.toString) {
          ret = v.toString();
        }
        if (v.label) {
          ret = v.label;
        }
      }
    }
    return ret;
  }

  function getValueForMatch(match, key, model) {
    var ret = undefined;
    if (match) {
      if (match.index) {
        var v = model[key];
        if (v) {
          var indexed = v[match.index];
          if (Array.isArray(v) && v.length > 0 && v[0] && v[0].name && v[0].type && v[0].valueString // can't do TypeScript instanceOf() at runtime
          && typeof match.index === 'string') {
            // special case, handle named event parameters
            var parm = v.filter(function (p) {
              return p.name === match.index;
            })[0];
            ret = getValue(parm);
          } else {
            ret = getValue(indexed);
          }
        } else {
          console.warn('Indexed match ($' + key + '[' + match.index + ']) does not exist.');
          ret = failed;
        }
      } else {
        ret = getValue(model[key]);
      }
    }
    return ret;
  }

  function replace(text, match, value) {
    var escaped = new RegExp(match.replace(escapeRE, '\\$&'), 'g');
    return text.replace(escaped, value);
  }

  _export('replace', replace);

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

      failed = Symbol('failed');
      escapeRE = /[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g;

      _export('CustomAction', CustomAction = function () {
        function CustomAction(data, url) {
          _classCallCheck(this, CustomAction);

          if (url) {
            this.label = data;
            this.url = url;
          } else {
            this.url = data.url;
            this.label = data.label;
          }
          if (!this.label || !this.url) {
            throw new Error('label and url are required!');
          }
          Object.freeze(this);
        }

        _createClass(CustomAction, [{
          key: 'interpolate',
          value: function interpolate(model) {
            var interpolated = this.url;
            if (model) {
              var _iteratorNormalCompletion = true;
              var _didIteratorError = false;
              var _iteratorError = undefined;

              try {
                for (var _iterator = Object.keys(model)[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
                  var key = _step.value;

                  var match = getMatch(interpolated, key);
                  if (match) {
                    var value = getValueForMatch(match, key, model);
                    if (value === failed) {
                      value = '';
                    }

                    value = encodeURIComponent(value);
                    interpolated = replace(interpolated, match.token, value);
                  }
                }
              } catch (err) {
                _didIteratorError = true;
                _iteratorError = err;
              } finally {
                try {
                  if (!_iteratorNormalCompletion && _iterator.return) {
                    _iterator.return();
                  }
                } finally {
                  if (_didIteratorError) {
                    throw _iteratorError;
                  }
                }
              }
            }
            return interpolated;
          }
        }, {
          key: 'validate',
          value: function validate(model) {
            var interpolated = this.url;
            var passed = true;
            if (model) {
              var _iteratorNormalCompletion2 = true;
              var _didIteratorError2 = false;
              var _iteratorError2 = undefined;

              try {
                for (var _iterator2 = Object.keys(model)[Symbol.iterator](), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
                  var key = _step2.value;

                  var match = getMatch(interpolated, key);
                  var value = getValueForMatch(match, key, model);
                  if (value === failed) {
                    passed = false;
                  }
                }
              } catch (err) {
                _didIteratorError2 = true;
                _iteratorError2 = err;
              } finally {
                try {
                  if (!_iteratorNormalCompletion2 && _iterator2.return) {
                    _iterator2.return();
                  }
                } finally {
                  if (_didIteratorError2) {
                    throw _iteratorError2;
                  }
                }
              }
            }
            return passed;
          }
        }, {
          key: 'open',
          value: function open(model) {
            var interpolated = this.interpolate(model);
            console.log('opening: ' + interpolated);
            window.open(interpolated, '_blank');
          }
        }]);

        return CustomAction;
      }());

      _export('CustomAction', CustomAction);
    }
  };
});
//# sourceMappingURL=custom_action.js.map

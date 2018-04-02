'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Gfuncs = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var index = [];
var categories = {
  Combine: [],
  Filter: [],
  Transform: []
};

function addFuncDef(funcDef) {
  funcDef.params = funcDef.params || [];
  funcDef.defaultParams = funcDef.defaultParams || [];

  if (funcDef.category) {
    funcDef.category.push(funcDef);
  }
  index[funcDef.name] = funcDef;
  index[funcDef.shortName || funcDef.name] = funcDef;
}

// Combine

addFuncDef({
  name: 'topN',
  category: categories.Combine,
  params: [{ name: "n", type: "int" }],
  defaultParams: [10]
});

addFuncDef({
  name: 'includeOther',
  category: categories.Combine
});

// Filter

addFuncDef({
  name: 'withExporterNode',
  category: categories.Filter,
  params: [{ name: "nodeCriteria", type: "string" }]
});

addFuncDef({
  name: 'withIfIndex',
  category: categories.Filter,
  params: [{ name: "ifIndex", type: "int" }]
});

// Transform

addFuncDef({
  name: 'perSecond',
  category: categories.Transform
});

addFuncDef({
  name: 'negativeEgress',
  category: categories.Transform
});

addFuncDef({
  name: 'negativeIngress',
  category: categories.Transform
});

addFuncDef({
  name: 'asTableSummary',
  category: categories.Transform
});

addFuncDef({
  name: 'combineIngressEgress',
  category: categories.Transform
});

_lodash2.default.each(categories, function (funcList, catName) {
  categories[catName] = _lodash2.default.sortBy(funcList, 'name');
});

function FuncInstance(funcDef, options) {
  this.def = funcDef;
  this.params = [];

  if (options && options.withDefaultParams) {
    this.params = funcDef.defaultParams.slice(0);
  }

  this.updateText();
}

FuncInstance.prototype.render = function (metricExp) {
  return {
    name: this.def.name,
    parameters: _lodash2.default.map(this.params, function (value, index) {
      var paramType = this.def.params[index].type;
      if (paramType === 'int' || paramType === 'value_or_series' || paramType === 'boolean') {
        return value;
      } else if (paramType === 'int_or_interval' && $.isNumeric(value)) {
        return value;
      }
      return value;
    }.bind(this))
  };
};

FuncInstance.prototype._hasMultipleParamsInString = function (strValue, index) {
  if (strValue.indexOf(',') === -1) {
    return false;
  }

  return this.def.params[index + 1] && this.def.params[index + 1].optional;
};

FuncInstance.prototype.updateParam = function (strValue, index) {
  // handle optional parameters
  // if string contains ',' and next param is optional, split and update both
  if (this._hasMultipleParamsInString(strValue, index)) {
    _lodash2.default.each(strValue.split(','), function (partVal, idx) {
      this.updateParam(partVal.trim(), index + idx);
    }.bind(this));
    return;
  }

  if (strValue === '' && this.def.params[index].optional) {
    this.params.splice(index, 1);
  } else {
    this.params[index] = strValue;
  }

  this.updateText();
};

FuncInstance.prototype.updateText = function () {
  if (this.params.length === 0) {
    this.text = this.def.name + '()';
    return;
  }

  var text = this.def.name + '(';
  text += this.params.join(', ');
  text += ')';
  this.text = text;
};

var Gfuncs = exports.Gfuncs = function () {
  function Gfuncs() {
    _classCallCheck(this, Gfuncs);
  }

  _createClass(Gfuncs, null, [{
    key: 'createFuncInstance',
    value: function createFuncInstance(funcDef, options) {
      if (_lodash2.default.isString(funcDef)) {
        if (!index[funcDef]) {
          throw { message: 'Method not found ' + name };
        }
        funcDef = index[funcDef];
      }
      return new FuncInstance(funcDef, options);
    }
  }, {
    key: 'getFuncDef',
    value: function getFuncDef(name) {
      return index[name];
    }
  }, {
    key: 'getCategories',
    value: function getCategories(graphiteVersion) {
      var filteredCategories = {};
      _lodash2.default.each(categories, function (functions, category) {
        if (functions.length) {
          filteredCategories[category] = functions;
        }
      });

      return filteredCategories;
    }
  }]);

  return Gfuncs;
}();
//# sourceMappingURL=flow_functions.js.map

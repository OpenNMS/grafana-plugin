'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.Clause = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _UI = require('../UI');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Clause = exports.Clause = function () {
    function Clause(uiSegmentSrv, operator, restriction) {
        _classCallCheck(this, Clause);

        if (restriction instanceof _UI.UI.RestrictionDTO) {
            restriction = new _UI.UI.Restriction(uiSegmentSrv, restriction);
        }
        this.uiSegmentSrv = uiSegmentSrv;
        this.restriction = restriction;
        this.controls = [];
        this.setOperator(operator);
    }

    _createClass(Clause, [{
        key: 'setOperator',
        value: function setOperator(newOperator) {
            // initialize if not initialized
            if (this.operator === void 0) {
                this.operator = this.uiSegmentSrv.newCondition(newOperator);
            }
            // Update the value if already initialized
            this.operator.value = newOperator;
        }
    }, {
        key: 'updateControls',
        value: function updateControls(query) {
            var controls = [new _UI.UI.Controls.RemoveControl(), new _UI.UI.Controls.AddControl(), new _UI.UI.Controls.AddNestedControl()];

            var self = this;
            this.controls = _lodash2.default.filter(controls, function (control) {
                return control.filter(query, self);
            });

            if (this.restriction instanceof _UI.UI.Query) {
                this.restriction.updateControls();
            }
        }
    }]);

    return Clause;
}();
//# sourceMappingURL=Clause.js.map

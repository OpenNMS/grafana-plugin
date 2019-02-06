'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.ClauseMapping = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _OperatorMapping = require('./OperatorMapping');

var _RestrictionMapping = require('./RestrictionMapping');

var _UI = require('../UI');

var _opennms = require('../../../opennms');

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var ClauseMapping = exports.ClauseMapping = function () {
    function ClauseMapping(uiSegmentSrv) {
        _classCallCheck(this, ClauseMapping);

        this.uiSegmentSrv = uiSegmentSrv;
    }

    _createClass(ClauseMapping, [{
        key: 'getUiClause',
        value: function getUiClause(apiClause) {
            if (!(apiClause instanceof _opennms.API.Clause)) {
                throw new TypeError("apiClause is not of type API.Clause");
            }
            var uiOperator = new _OperatorMapping.OperatorMapping().getUiOperator(apiClause.operator);
            var uiRestriction = new _RestrictionMapping.RestrictionMapping(this.uiSegmentSrv).getUiRestriction(apiClause.restriction);
            return new _UI.UI.Clause(this.uiSegmentSrv, uiOperator, uiRestriction);
        }
    }, {
        key: 'getApiClause',
        value: function getApiClause(uiClause) {
            if (!(uiClause instanceof _UI.UI.Clause)) {
                throw new TypeError("uiClause is not of type UI.Clause");
            }
            var apiOperator = new _OperatorMapping.OperatorMapping().getApiOperator(uiClause.operator.value);
            var apiRestriction = new _RestrictionMapping.RestrictionMapping(this.uiSegmentSrv).getApiRestriction(uiClause.restriction);
            if (apiRestriction !== null) {
                return new _opennms.API.Clause(apiRestriction, apiOperator);
            }
            return null;
        }
    }]);

    return ClauseMapping;
}();
//# sourceMappingURL=ClauseMapping.js.map

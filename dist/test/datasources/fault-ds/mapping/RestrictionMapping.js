'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.RestrictionMapping = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _ComparatorMapping = require('./ComparatorMapping');

var _ClauseMapping = require('./ClauseMapping');

var _AttributeMapping = require('./AttributeMapping');

var _ValueMapping = require('./ValueMapping');

var _UI = require('../UI');

var _opennms = require('../../../opennms');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var RestrictionMapping = exports.RestrictionMapping = function () {
    function RestrictionMapping(uiSegmentSrv) {
        _classCallCheck(this, RestrictionMapping);

        this.uiSegmentSrv = uiSegmentSrv;
    }

    _createClass(RestrictionMapping, [{
        key: 'getUiRestriction',
        value: function getUiRestriction(apiRestriction) {
            if (apiRestriction instanceof _opennms.API.NestedRestriction) {
                return this.getUiQuery(apiRestriction);
            } else {
                var uiRestriction = new _UI.UI.Restriction(this.uiSegmentSrv, new _UI.UI.RestrictionDTO(new _AttributeMapping.AttributeMapping().getUiAttribute(apiRestriction.attribute), new _ComparatorMapping.ComparatorMapping().getUiComparator(apiRestriction.comparator), new _ValueMapping.ValueMapping().getUiValue(apiRestriction.attribute, apiRestriction.value)));
                return uiRestriction;
            }
        }
    }, {
        key: 'getUiQuery',
        value: function getUiQuery(apiNestedRestriction) {
            var self = this;
            var uiQuery = new _UI.UI.Query(this.uiSegmentSrv);
            var uiClauses = _lodash2.default.map(apiNestedRestriction.clauses, function (clause) {
                return new _ClauseMapping.ClauseMapping(self.uiSegmentSrv).getUiClause(clause);
            });
            _lodash2.default.each(uiClauses, function (uiClause) {
                uiQuery.addClause(uiClause);
            });
            return uiQuery;
        }
    }, {
        key: 'getApiRestriction',
        value: function getApiRestriction(uiRestriction) {
            if (uiRestriction instanceof _UI.UI.Query) {
                return this.getApiNestedRestriction(uiRestriction);
            } else {
                var restrictionDTO = uiRestriction.asRestrictionDTO();
                if (restrictionDTO !== null) {
                    var attribute = new _AttributeMapping.AttributeMapping().getApiAttribute(restrictionDTO.attribute);
                    var comparator = new _ComparatorMapping.ComparatorMapping().getApiComparator(restrictionDTO.comparator);
                    var value = new _ValueMapping.ValueMapping().getApiValue(restrictionDTO.attribute, restrictionDTO.value);
                    return new _opennms.API.Restriction(attribute, comparator, value);
                }
                return null;
            }
        }
    }, {
        key: 'getApiNestedRestriction',
        value: function getApiNestedRestriction(uiQuery) {
            var self = this;
            var nestedRestriction = new _opennms.API.NestedRestriction();
            _lodash2.default.each(uiQuery.clauses, function (uiClause) {
                var apiClause = new _ClauseMapping.ClauseMapping(self.uiSegmentSrv).getApiClause(uiClause);
                if (apiClause !== null) {
                    nestedRestriction.withClause(apiClause);
                }
            });
            return nestedRestriction;
        }
    }]);

    return RestrictionMapping;
}();
//# sourceMappingURL=RestrictionMapping.js.map

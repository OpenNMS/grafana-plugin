'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.FilterMapping = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _opennms = require('../../../opennms');

var _UI = require('../UI');

var _ClauseMapping = require('./ClauseMapping');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Maps a UiFilter to API.filter and vice versa.
 */
var FilterMapping = exports.FilterMapping = function () {
    function FilterMapping(uiSegmentSrv) {
        _classCallCheck(this, FilterMapping);

        this.uiSegmentSrv = uiSegmentSrv;
    }

    _createClass(FilterMapping, [{
        key: 'getApiFilter',
        value: function getApiFilter(uiFilter) {
            // Ensure we can migrate
            if (!uiFilter instanceof _UI.UI.Filter) {
                throw new TypeError("uiFilter is not of type UI.Filter.");
            }

            var self = this;
            var filter = new _opennms.API.Filter();
            filter.limit = 0;

            _lodash2.default.each(uiFilter.query.clauses, function (eachClause) {
                var apiClause = new _ClauseMapping.ClauseMapping(self.uiSegmentSrv).getApiClause(eachClause);
                if (apiClause !== null) {
                    filter.withClause(apiClause);
                }
            });
            return filter;
        }
    }, {
        key: 'getUiFilter',
        value: function getUiFilter(apiFilter) {
            if (!apiFilter instanceof _opennms.API.Filter) {
                throw new TypeError("apiFilter is not of type API.Filter");
            }

            var self = this;
            var uiFilter = new _UI.UI.Filter(this.uiSegmentSrv);

            _lodash2.default.each(apiFilter.clauses, function (apiClause) {
                var uiClause = new _ClauseMapping.ClauseMapping(self.uiSegmentSrv).getUiClause(apiClause);
                uiFilter.addClause(uiClause);

                // set parentQuery for all nested queries
                self.applyParentQuery(uiClause, uiFilter.query);
            });

            return uiFilter;
        }
    }, {
        key: 'applyParentQuery',
        value: function applyParentQuery(clause, parentQuery) {
            if (clause.restriction instanceof _UI.UI.Query) {
                clause.restriction.parentQuery = parentQuery;
                this.applyParentQuery(clause.restriction.clauses, clause.restriction);
            }
        }
    }]);

    return FilterMapping;
}();
//# sourceMappingURL=FilterMapping.js.map

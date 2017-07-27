'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.Filter = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _opennms = require('../../../opennms');

var _Mapping = require('../Mapping');

var _UI = require('../UI');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Filter = exports.Filter = function () {
    function Filter(uiSegmentSrv) {
        _classCallCheck(this, Filter);

        this.uiSegmentSrv = uiSegmentSrv;
        this.query = new _UI.UI.Query(uiSegmentSrv);
        this.query.root = true;
    }

    _createClass(Filter, [{
        key: 'updateControls',
        value: function updateControls() {
            this.query.updateControls();
        }
    }, {
        key: 'getQueryString',
        value: function getQueryString() {
            var string = "select all alarms";
            if (this.query.isEmpty()) {
                return string;
            }

            var queryString = this.query.asString();
            if (queryString && queryString.length > 0) {
                return string + " where " + queryString;
            }
            return string;
        }
    }, {
        key: 'clear',
        value: function clear() {
            this.query.clear();
        }
    }, {
        key: 'addClause',
        value: function addClause(clause) {
            if (clause instanceof _opennms.API.Clause) {
                var uiClause = new _Mapping.Mapping.ClauseMapping(this.uiSegmentSrv).getUiClause(clause);
                this.query.addClause(uiClause);
            } else if (clause instanceof _UI.UI.Clause) {
                this.query.addClause(clause);
            } else {
                throw new Error("Clause type is not supported");
            }
        }
    }, {
        key: 'withClause',
        value: function withClause(clause) {
            this.addClause(clause);
            return this;
        }
    }, {
        key: 'removeClause',
        value: function removeClause(clause) {
            this.query.removeClause(clause);
        }
    }]);

    return Filter;
}();
//# sourceMappingURL=Filter.js.map

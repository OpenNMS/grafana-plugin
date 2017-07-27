'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.RestrictionDTO = exports.Restriction = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Restriction = exports.Restriction = function () {
    function Restriction(uiSegmentSrv, restriction) {
        _classCallCheck(this, Restriction);

        this.uiSegmentSrv = uiSegmentSrv;
        this.segments = [];
        if (restriction) {
            this.setAttribute(restriction.attribute);
            this.setComparator(restriction.comparator);
            this.setValue(restriction.value);
        }
    }

    _createClass(Restriction, [{
        key: 'getSegmentCount',
        value: function getSegmentCount() {
            return this.segments.length;
        }
    }, {
        key: 'removeLastSegment',
        value: function removeLastSegment() {
            this.segments.pop();
        }
    }, {
        key: 'addSegment',
        value: function addSegment(segment) {
            if (segment) {
                this.segments.push(segment);
            }
        }
    }, {
        key: 'setAttribute',
        value: function setAttribute(attribute) {
            if (this.segments.length == 0) {
                this.segments.push({});
            }
            this.segments[0] = this.uiSegmentSrv.newKey(attribute);
        }
    }, {
        key: 'setComparator',
        value: function setComparator(comparator) {
            if (this.segments.length == 1) {
                this.segments.push({});
            }
            this.segments[1] = this.uiSegmentSrv.newOperator(comparator);
        }
    }, {
        key: 'setValue',
        value: function setValue(value) {
            if (this.segments.length == 2) {
                this.segments.push({});
            }
            this.segments[2] = this.uiSegmentSrv.newKeyValue(value);
        }
    }, {
        key: 'asRestrictionDTO',
        value: function asRestrictionDTO() {
            var segments = _lodash2.default.filter(this.segments, function (segment) {
                return segment.type !== 'plus-button' && (segment.fake === undefined || segment.fake === false);
            });
            if (segments.length > 0 && segments.length % 3 == 0) {
                var data = {};
                _lodash2.default.each(segments, function (segment) {
                    if (segment.type === 'key') {
                        data.attribute = segment.value;
                    } else if (segment.type === 'operator') {
                        data.comparator = segment.value;
                    } else if (segment.type === 'value') {
                        data.value = segment.value;
                    }
                });
                return new RestrictionDTO(data.attribute, data.comparator, data.value);
            }
            return null;
        }
    }, {
        key: 'asString',
        value: function asString() {
            var getComparator = function getComparator(restriction) {
                if (restriction.value === 'null') {
                    if (restriction.comparator === '=') {
                        return "is";
                    }
                    if (restriction.comparator === '!=') {
                        return "is not";
                    }
                }
                return restriction.comparator;
            };

            var getValue = function getValue(restriction) {
                if (restriction.value === 'null') {
                    return 'null';
                }
                return "'" + restriction.value + "'";
            };

            var restriction = this.asRestrictionDTO();
            if (restriction !== null) {
                return restriction.attribute + " " + getComparator(restriction) + " " + getValue(restriction);
            }
            return null;
        }
    }]);

    return Restriction;
}();

// Just use for internal use


var RestrictionDTO = exports.RestrictionDTO = function RestrictionDTO(attribute, comparator, value) {
    _classCallCheck(this, RestrictionDTO);

    this.attribute = attribute;
    this.comparator = comparator;
    this.value = value;
};
//# sourceMappingURL=Restriction.js.map

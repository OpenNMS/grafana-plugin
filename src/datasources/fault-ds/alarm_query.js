import _ from 'lodash';
import {OnmsSeverity} from './severities';

export class AlarmQuery {

    constructor(restrictions)  {
        this.restrictions = restrictions;
    }

    render() {
        let query = "select all alarms";
        if (this.restrictions.length == 0) {
            return query;
        }
        let restrictionText = this.renderRestrictions();
        return query + " WHERE " + restrictionText;
    }

    renderRestrictions() {
        var getComparator = function(restriction) {
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

        var getValue = function(restriction) {
            if (restriction.value === 'null') {
                return 'null';
            }
            return "'" + restriction.value + "'";
        };

        var query = _.map(this.restrictions, function(restriction) {
            var string = restriction.attribute + " " + getComparator(restriction) + " " + getValue(restriction);
            return string;
        }).join(" AND ");
        return query;
    }

    getAttributeName(attribute) {
        var mapping = {
            'location': 'node.location.locationName',
            'service': 'service',
            'ipAddress': 'ipAddr',
        };

        var mappedAttribute = mapping[attribute];
        if (mappedAttribute) {
            return mappedAttribute;
        }
        return attribute;
    };

    getRestrictionsAsFIQL() {
        var map = {
            '=': '==',
            '<=': '=le=',
            '>=': '=ge=',
            '>': '=gt=',
            '<': '=lt='
        };

        var getComparator = function(comparator) {
            if (map[comparator]) {
                return map[comparator];
            }
            return comparator;
        };

        var escapeSearchValue = function(value) {
            return encodeURIComponent(value);
        };

        var getValue = function(attribute, value) {
            if (attribute === 'alarmAckTime') {
                if (value === 'null') {
                    value = new Date(0);
                }

                // yyyy-MM-ddTHH:mm:ss.sssZ
                // 2017-06-08T10:17:17.173+0200
                value = value.toJSON().replace("Z", "+0000"); // make it parsable by java
            }
            if ("null" === value) {
                value = '\u0000';
            }
            return escapeSearchValue(value)
        };

        var fiql = _.map(this.restrictions, restriction => {
            var attribute = this.getAttributeName(restriction.attribute);
            var comparator = getComparator(restriction.comparator);
            var value = getValue(restriction.attribute, restriction.value);
            return [attribute, comparator, value].join("");
        }).join(";");
        return fiql;
    }
}
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
        var query = _.map(this.restrictions, function(restriction) {
            var string = restriction.attribute + " " + restriction.comparator + " " + "'" + restriction.value + "'";
            return string;
        }).join(" AND ");
        return query;
    }

    getRestrictionsAsQuery() {
        var mapping = {
            'location': 'node.location.locationName',
            'serviceType': 'serviceType.name',
        };

        var getAttributeName = function(attribute) {
            var mappedAttribute = mapping[attribute];
            if (mappedAttribute) {
                return mappedAttribute;
            }
            return attribute;
        };

        var getAttributeValue = function(restriction) {
            if (restriction.attribute === 'severity') {
                return new OnmsSeverity().getSeverityByLabel(restriction.value).id;
            }
            return '%' + restriction.value + '%';
        };

        // var restrictionAsQuery = _.map(this.restrictions, restriction => {
        //     var attribute = getAttributeName(restriction.attribute);
        //     var value = getAttributeValue(restriction);
        //     var string = [attribute, restriction.comparator, value].join(" ");
        //     return string;
        // }).join(" AND ");

        var keys = _.map(this.restrictions, restriction => {
            return getAttributeName(restriction.attribute);
        });
        var values = _.map(this.restrictions, restriction => {
           return getAttributeValue(restriction);
        });

        var parameters = _.zipObject(keys, values);
        console.log(parameters);
        return parameters;
    }
}
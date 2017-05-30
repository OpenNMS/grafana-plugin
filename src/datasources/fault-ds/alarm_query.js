import _ from 'lodash';

export class AlarmQuery {

    constructor(restrictions, attributes)  {
        this.restrictions = restrictions;
        this.attributes = attributes;
    }

    render() {
        var self = this;
        var query = _.map(this.restrictions, function(restriction) {
            console.log(restriction);
            var value = self.getValue(restriction);
            var string = restriction.attribute + " " + restriction.comparator + " " + value;
            return string;
        }).join(" AND ");
        return query;
    }

    getValue(restriction) {
        var attribute = this.getAttribute(restriction.attribute);
        console.log(attribute);
        if (attribute.type == 'number') {
            return restriction.value;
        }
        return "'" + restriction.value + "'";
    }

    getAttribute(name) {
        return _.find(this.attributes, function(eachAttribute) {
            return eachAttribute.name === name;
        });
    }
}
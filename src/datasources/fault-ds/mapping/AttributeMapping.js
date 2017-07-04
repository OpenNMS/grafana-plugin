import _ from 'lodash';

export class AttributeMapping {
    constructor() {
        this.attributeMapping = {
            'location': 'node.location.locationName',
            'service': 'service',
            'ipAddress': 'ipAddr',
            'severity': 'alarm.severity'
        };
    }

    getUiAttribute(externalAttribute) {
        const internalAttribute = _.findKey(this.attributeMapping, function(value) {
            return value === externalAttribute;
        });
        return internalAttribute || externalAttribute;
    }

    getApiAttribute(internalAttribute) {
        return this.attributeMapping[internalAttribute] || internalAttribute;
    }
}
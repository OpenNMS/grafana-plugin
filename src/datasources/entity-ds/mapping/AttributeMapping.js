import _ from 'lodash';

export class AttributeMapping {
    constructor() {
        this.attributeMapping = {
            'location': 'location.locationName',
            'service': 'serviceType.name',
            'category': 'category.name',
            'ipAddr': 'ipInterface.ipAddress',
            'ipAddress': 'ipInterface.ipAddress',
            'severity': 'severity'
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
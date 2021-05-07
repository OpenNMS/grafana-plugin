import _ from 'lodash';

export class AttributeMapping {
    constructor(public attributeMapping: any) {
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

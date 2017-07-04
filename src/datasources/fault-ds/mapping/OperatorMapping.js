import _ from 'lodash';
import {API} from '../../../opennms'

export class OperatorMapping {
    getUiOperator(apiOperator) {
        const theOperator = API.Operators[apiOperator.label];
        if (theOperator && theOperator.label) {
            return theOperator.label;
        }
        throw { message: "No operator found with label " + apiOperator.label, operators: API.Operators};
    }

    getApiOperator(uiOperator) {
        const apiOperator = _.find(API.Operators, function(eachOperator) {
            return eachOperator.matches(uiOperator);
        });
        if (!apiOperator) {
            throw new Error("Could not map uiOperator '" + uiOperator + "' to API operator.");
        }
        return apiOperator;
    }
}
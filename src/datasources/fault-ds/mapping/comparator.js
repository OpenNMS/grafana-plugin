import _ from 'lodash';
import {API} from '../../../opennms';
import {UI} from '../ui';

export class ComparatorMapping {
    // TODO MVR this is not really a good way
    getUiComparator(apiComparator) {
        const theComparator = API.Comparators[apiComparator.label];
        if (theComparator.aliases && theComparator.aliases.length > 0) {
            return theComparator.aliases[0];
        }
        throw new Error("No matching UI comparator found for '" + apiComparator.label + "'.");
    }

    getApiComparator(uiComparator) {
        const apiComparator = _.find(API.Comparators, function(comparator) {
            return comparator.matches(uiComparator);
        });
        if (!apiComparator) {
            throw new Error("No API comparator for alias '" + uiComparator + "' found.");
        }
        return apiComparator;
    }
}

import _ from 'lodash';
import {API} from '../../../opennms';

export class ComparatorMapping {
    getUiComparator(apiComparator) {
        const theComparator = API.Comparators[apiComparator.label];
        if (theComparator !== API.Comparators.NULL
            && theComparator !== API.Comparators.NOTNULL
            && theComparator != API.LIKE
            && theComparator != API.ILIKE
            && theComparator.aliases && theComparator.aliases.length > 0) {
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

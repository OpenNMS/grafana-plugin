import _ from 'lodash'
import {API} from '../../opennms'

/**
 * The filter may be reloaded from a persisted state.
 * The internal opennms.js API requires a concrete implementation of Comparators or Operators in order to work.
 * As the object was persisted, the references DO NOT MATCH. In order to make them match, we just rebuild the filter.
 */
export class FilterCloner {
    cloneFilter(filter) {
        const newFilter = new API.Filter();
        newFilter.limit = filter.limit;
        newFilter.clauses = this.cloneNestedRestriction(filter).clauses;
        return newFilter;
    }

    cloneClause(clause) {
        const operator = _.find(API.Operators, function(operator) {
            return operator.label === clause.operator.label;
        });

        // Nested restriction
        if (clause.restriction.clauses) {
            const nestedRestriction = this.cloneNestedRestriction(clause.restriction);
            return new API.Clause(nestedRestriction, operator);
        } else { // Normal restriction
            const restriction = this.cloneRestriction(clause.restriction);
            return new API.Clause(restriction, operator);
        }
    }

    cloneNestedRestriction(nestedRestriction) {
        const self = this;
        const newNestedRestriction = new API.NestedRestriction();
        _.each(nestedRestriction.clauses, function(clause) {
            newNestedRestriction.withClause(self.cloneClause(clause));
        });
        return newNestedRestriction;
    }

    cloneRestriction(restriction) {
        const comparator = _.find(API.Comparators, function(comparator) {
            return comparator.label === restriction.comparator.label;
        });
        return new API.Restriction(restriction.attribute, comparator, restriction.value);
    }
}
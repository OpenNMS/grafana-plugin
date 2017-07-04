import _ from 'lodash';

import {UiRestriction, UiQuery, UiClause, Restriction} from '../ui';
import {ComparatorMapping} from './comparator'
import {ClauseMapping} from './clause'
import {AttributeMapping} from './attribute';
import {ValueMapping} from './value';
import {API} from '../../../opennms';
import {UI} from '../ui'

export class RestrictionMapping {

    constructor(uiSegmentSrv) {
        this.uiSegmentSrv = uiSegmentSrv;
    }

    getUiRestriction(apiRestriction) {
        if (apiRestriction instanceof API.NestedRestriction) {
            return this.getUiQuery(apiRestriction);
        } else {
            // TODO MVR Only consider values which are set up correctly
            var uiRestriction = new UI.Restriction(
                this.uiSegmentSrv,
                new UI.RestrictionDTO(
                    new AttributeMapping().getUiAttribute(apiRestriction.attribute),
                    new ComparatorMapping().getUiComparator(apiRestriction.comparator),
                    new ValueMapping().getUiValue(apiRestriction.attribute, apiRestriction.value)
                )
            );
            return uiRestriction
        }
    }

    getUiQuery(apiNestedRestriction) {
        let self = this;
        let uiQuery = new UI.Query(this.uiSegmentSrv);
        var uiClauses = _.map(apiNestedRestriction.clauses, clause => {
            return new ClauseMapping(self.uiSegmentSrv).getUiClause(clause);
        });
        _.each(uiClauses, uiClause => {
            uiQuery.addClause(uiClause);
        });
        return uiQuery;
    }

    getApiRestriction(uiRestriction) {
        if (uiRestriction instanceof UI.Query) {
            return this.getApiNestedRestriction(uiRestriction);
        } else {
            const restrictionDTO = uiRestriction.asRestrictionDTO();
            if (restrictionDTO !== null) {
                const attribute = new AttributeMapping().getApiAttribute(restrictionDTO.attribute);
                const comparator = new ComparatorMapping().getApiComparator(restrictionDTO.comparator);
                const value = new ValueMapping().getApiValue(restrictionDTO.attribute, restrictionDTO.value);
                return new API.Restriction(attribute, comparator, value);
            }
            return null;
        }
    }

    getApiNestedRestriction(uiQuery) {
        const self = this;
        const nestedRestriction = new API.NestedRestriction();
        _.each(uiQuery.clauses, uiClause => {
            const apiClause = new ClauseMapping(self.uiSegmentSrv).getApiClause(uiClause);
            if (apiClause !== null) {
                nestedRestriction.withClause(apiClause);
            }
        });
        return nestedRestriction;
    }
}
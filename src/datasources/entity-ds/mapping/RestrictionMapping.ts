import _ from 'lodash';

import { API } from 'opennms';

import Entity from '../Entity';

import { Restriction, RestrictionDTO } from '../ui/Restriction';
import { Query } from '../ui/Query';

import { ClauseMapping } from './ClauseMapping'
import { ComparatorMapping } from './ComparatorMapping'
import { ValueMapping } from './ValueMapping';

export class RestrictionMapping {

    constructor(public uiSegmentSrv: any, public entity: Entity) {
    }

    getUiRestriction(apiRestriction) {
        if (apiRestriction instanceof API.NestedRestriction) {
            return this.getUiQuery(apiRestriction);
        } else {
            const uiRestriction = new Restriction(
                this.uiSegmentSrv,
                new RestrictionDTO(
                    this.entity.getAttributeMapping().getUiAttribute(apiRestriction.attribute),
                    new ComparatorMapping().getUiComparator(apiRestriction.comparator),
                    new ValueMapping().getUiValue(apiRestriction.attribute, apiRestriction.value)
                )
            );
            return uiRestriction
        }
    }

    getUiQuery(apiNestedRestriction) {
        let self = this;
        let uiQuery = new Query(this.uiSegmentSrv);
        const uiClauses = _.map(apiNestedRestriction.clauses, clause => {
            return new ClauseMapping(self.uiSegmentSrv, self.entity).getUiClause(clause);
        });
        _.each(uiClauses, uiClause => {
            uiQuery.addClause(uiClause);
        });
        return uiQuery;
    }

    getApiRestriction(uiRestriction: Query | Restriction) {
        if (uiRestriction instanceof Query) {
            return this.getApiNestedRestriction(uiRestriction);
        } else {
            const restrictionDTO = uiRestriction.asRestrictionDTO();
            if (restrictionDTO !== null) {
                const attribute = this.entity.getAttributeMapping().getApiAttribute(restrictionDTO.attribute);
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
            const apiClause = new ClauseMapping(self.uiSegmentSrv, self.entity).getApiClause(uiClause);
            if (apiClause !== null) {
                nestedRestriction.withClause(apiClause);
            }
        });
        return nestedRestriction;
    }
}

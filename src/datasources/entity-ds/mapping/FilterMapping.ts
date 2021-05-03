import _ from 'lodash';
import { API } from 'opennms'

import Entity from '../Entity';
import { UI } from '../UI';

import { ClauseMapping } from './ClauseMapping';
import { OrderByMapping } from './OrderByMapping';

/**
 * Maps a UiFilter to API.filter and vice versa.
 */
export class FilterMapping {
    constructor(public uiSegmentSrv: any, public entity: Entity) {
    }

    getApiFilter(uiFilter) {
        // Ensure we can migrate
        if (!(uiFilter instanceof UI.Filter)) {
            throw new TypeError("uiFilter is not of type UI.Filter.");
        }

        const filter = new API.Filter();
        filter.limit = 0;
        filter.orderBy = [];

        _.each(uiFilter.query.clauses, (eachClause) => {
            const apiClause = new ClauseMapping(this.uiSegmentSrv, this.entity).getApiClause(eachClause);
            if (apiClause !== null) {
                filter.withClause(apiClause);
            }
        });

        _.each(uiFilter.query.orderBy, (eachOrderBy) => {
            if (eachOrderBy.isFake()) {
                return;
            }
            const apiOrderBy = new OrderByMapping(this.uiSegmentSrv, this.entity).getApiOrderBy(eachOrderBy, uiFilter.query.order.label);
            if (apiOrderBy != null) {
                filter.withOrderBy(apiOrderBy);
            }
        });

        return filter;
    }

    getUiFilter(apiFilter) {
        if (!(apiFilter instanceof API.Filter)) {
            throw new TypeError("apiFilter is not of type API.Filter");
        }

        let uiFilter = new UI.Filter(this.uiSegmentSrv, this.entity);

        _.each(apiFilter.clauses, (apiClause) => {
            const uiClause = new ClauseMapping(this.uiSegmentSrv, this.entity).getUiClause(apiClause);
            uiFilter.addClause(uiClause);

            // set parentQuery for all nested queries
            this.applyParentQuery(uiClause, uiFilter.query);
        });

        _.each(apiFilter.orderBy, (apiOrderBy) => {
            const uiOrderBy = new OrderByMapping(this.uiSegmentSrv, this.entity).getUiOrderBy(apiOrderBy);
            uiFilter.addOrderBy(uiOrderBy);
            uiFilter.query.setOrder(apiOrderBy.order.label);
        });

        return uiFilter;
    }

    applyParentQuery(clause, parentQuery) {
        if (clause.restriction instanceof UI.Query) {
            clause.restriction.parentQuery = parentQuery;
            this.applyParentQuery(clause.restriction.clauses, clause.restriction);
        }
    }
}

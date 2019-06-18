import _ from 'lodash';
import {API} from 'opennms'
import {UI} from '../UI';
import {ClauseMapping} from './ClauseMapping';

/**
 * Maps a UiFilter to API.filter and vice versa.
 */
export class FilterMapping {

    constructor(uiSegmentSrv, entity) {
        this.uiSegmentSrv = uiSegmentSrv;
        this.entity = entity;
    }

    getApiFilter(uiFilter) {
        // Ensure we can migrate
        if (!(uiFilter instanceof UI.Filter)) {
            throw new TypeError("uiFilter is not of type UI.Filter.");
        }

        const self = this;
        const filter = new API.Filter();
        filter.limit = 0;

        _.each(uiFilter.query.clauses, function(eachClause) {
            const apiClause = new ClauseMapping(self.uiSegmentSrv, self.entity).getApiClause(eachClause);
            if (apiClause !== null) {
                filter.withClause(apiClause);
            }
        });
        return filter;
    }

    getUiFilter(apiFilter) {
        if (!(apiFilter instanceof API.Filter)) {
            throw new TypeError("apiFilter is not of type API.Filter");
        }

        const self = this;
        let uiFilter = new UI.Filter(this.uiSegmentSrv, self.entity);

        _.each(apiFilter.clauses, apiClause => {
            const uiClause = new ClauseMapping(self.uiSegmentSrv, self.entity).getUiClause(apiClause);
            uiFilter.addClause(uiClause);

            // set parentQuery for all nested queries
            self.applyParentQuery(uiClause, uiFilter.query);

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
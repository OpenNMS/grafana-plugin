import _ from 'lodash';
import {API} from '../../../opennms'
import {UI} from '../ui';
import {ClauseMapping} from './clause';

/**
 * Maps a UiFilter to API.filter and vice versa.
 */
export class FilterMapping {

    constructor(uiSegmentSrv) {
        this.uiSegmentSrv = uiSegmentSrv;
    }

    getApiFilter(uiFilter) {
        // Ensure we can migrate
        if (!uiFilter instanceof UI.Filter) {
            throw new TypeError("uiFilter is not of type UI.Filter.");
        }

        const self = this;
        const filter = new API.Filter();
        filter.limit = uiFilter.limit;

        _.each(uiFilter.query.clauses, function(eachClause) {
            const apiClause = new ClauseMapping(self.uiSegmentSrv).getApiClause(eachClause);
            if (apiClause != null) {
                filter.withClause(apiClause);
            }
        });
        return filter;
    }

    getUiFilter(apiFilter) {
        if (!apiFilter instanceof API.Filter) {
            throw new TypeError("apiFilter is not of type API.Filter");
        }

        const self = this;
        let uiFilter = new UI.Filter(this.uiSegmentSrv);
        uiFilter.limit = apiFilter.limit;

        _.each(apiFilter.clauses, apiClause => {
            const uiClause = new ClauseMapping(self.uiSegmentSrv).getUiClause(apiClause);
            uiFilter.addClause(uiClause);
        });

        return uiFilter;
    }
}
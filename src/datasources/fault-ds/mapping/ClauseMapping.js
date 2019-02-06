import {OperatorMapping} from './OperatorMapping';
import {RestrictionMapping} from './RestrictionMapping';
import {UI} from '../UI';
import {API} from '../../../opennms'

export class ClauseMapping {
    constructor(uiSegmentSrv) {
        this.uiSegmentSrv = uiSegmentSrv;
    }

    getUiClause(apiClause) {
        if (!(apiClause instanceof API.Clause)) {
            throw new TypeError("apiClause is not of type API.Clause");
        }
        const uiOperator = new OperatorMapping().getUiOperator(apiClause.operator);
        const uiRestriction = new RestrictionMapping(this.uiSegmentSrv).getUiRestriction(apiClause.restriction);
        return new UI.Clause(this.uiSegmentSrv, uiOperator, uiRestriction);
    }

    getApiClause(uiClause) {
        if (!(uiClause instanceof UI.Clause)) {
            throw new TypeError("uiClause is not of type UI.Clause");
        }
        const apiOperator = new OperatorMapping().getApiOperator(uiClause.operator.value);
        const apiRestriction = new RestrictionMapping(this.uiSegmentSrv).getApiRestriction(uiClause.restriction);
        if (apiRestriction !== null) {
            return new API.Clause(apiRestriction, apiOperator);
        }
        return null;
    }
}

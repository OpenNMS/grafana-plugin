import {UI} from '../ui';

export class Clause {

    constructor(uiSegmentSrv, operator, restriction) {
        if (restriction instanceof UI.RestrictionDTO) {
            restriction = new UI.Restriction(uiSegmentSrv, restriction);
        }
        this.uiSegmentSrv = uiSegmentSrv;
        this.restriction = restriction;
        this.setOperator(operator);
    }

    addPlusButtonIfRequired() {
        if (this.restriction) {
            this.restriction.addPlusButtonIfRequired();
        }
    }

    setOperator(operator) {
        this.operator = this.uiSegmentSrv.newCondition(operator);
    }
}
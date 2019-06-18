import _ from 'lodash';
import {UI} from '../UI';

export class Clause {

    constructor(uiSegmentSrv, operator, restriction) {
        if (restriction instanceof UI.RestrictionDTO) {
            restriction = new UI.Restriction(uiSegmentSrv, restriction);
        }
        this.uiSegmentSrv = uiSegmentSrv;
        this.restriction = restriction;
        this.controls = [];
        this.setOperator(operator);
    }

    setOperator(newOperator) {
        // initialize if not initialized
        if (this.operator === void 0) {
            this.operator = this.uiSegmentSrv.newCondition(newOperator);
        }
        // Update the value if already initialized
        this.operator.value = newOperator;
    }

    updateControls(query) {
        const controls = [
            new UI.Controls.RemoveControl(),
            new UI.Controls.AddControl(),
            new UI.Controls.AddNestedControl()
        ];

        const self = this;
        this.controls = _.filter(controls, control => {
            return control.filter(query, self);
        });

        if (this.restriction instanceof UI.Query) {
            this.restriction.updateControls();
        }
    }
}
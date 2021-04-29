import _ from 'lodash';

import { Controls } from '../UI';
import { Control } from './Controls';
import { Query } from './Query';
import { Restriction, RestrictionDTO } from './Restriction';

export class Clause {
    controls = [] as Control[];
    restriction: Restriction|Query;

    constructor(public uiSegmentSrv: any, public operator: any, restriction: Restriction|RestrictionDTO|Query) {
        if (restriction instanceof RestrictionDTO) {
            this.restriction = new Restriction(uiSegmentSrv, restriction);
        } else {
            this.restriction = restriction;
        }
        this.setOperator(operator);
    }

    setOperator(newOperator: string) {
        if (this.operator && this.operator.value) {
            this.operator.value = newOperator;
            return;
        }

        // initialize if not initialized
        this.operator = this.uiSegmentSrv.newCondition(newOperator);
    }

    updateControls(query: any) {
        const controls = [
            new Controls.RemoveControl(),
            new Controls.AddControl(),
            new Controls.AddNestedControl()
        ];

        const self = this;
        this.controls = _.filter(controls, control => {
            return control.filter(query, self);
        });

        if (this.restriction instanceof Query) {
            this.restriction.updateControls();
        }
    }

    addSegment(segment: any) {
        if (this.restriction && this.restriction instanceof Restriction) {
            this.restriction.addSegment(segment);
        } else {
            throw new Error('Clause is a Query clause, not a restriction clause! Segments are not supported.');
        }
    }
}

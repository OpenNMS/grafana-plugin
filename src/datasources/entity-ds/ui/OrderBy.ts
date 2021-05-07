import {KEY_PLACEHOLDER} from '../constants';

export class OrderBy {
    segment: any;

    constructor(public uiSegmentSrv: any, attribute?: any, isFake?: boolean) {
        this.uiSegmentSrv = uiSegmentSrv;
        this.segment = undefined;

        if (attribute) {
            this.setAttribute(attribute);
            if (isFake) {
                this.segment.fake = true;
            }
        }
    }

    isFake() {
        return !!this.segment.fake;
    }

    getAttribute() {
        if (!this.segment) {
            return void 0;
        }
        return this.segment.value;
    }

    setAttribute(attribute) {
        if (attribute === KEY_PLACEHOLDER) {
            this.segment = this.uiSegmentSrv.newPlusButton();
        } else {
            this.segment = this.uiSegmentSrv.newKey(attribute);
            this.segment.fake = false;
        }
    }

    asString() {
        return this.getAttribute() || null;
    }
}

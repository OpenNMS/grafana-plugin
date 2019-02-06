import {API} from '../../../opennms';
import {Mapping} from '../Mapping';
import {UI} from '../UI';

export class Filter {
    constructor(uiSegmentSrv) {
        this.uiSegmentSrv = uiSegmentSrv;
        this.query = new UI.Query(uiSegmentSrv);
        this.query.root = true;
    }

    updateControls() {
        this.query.updateControls();
    }

    getQueryString() {
        let string = "select all alarms";
        if (this.query.isEmpty()) {
            return string;
        }

        let queryString = this.query.asString();
        if (queryString && queryString.length > 0) {
            return string + " where " + queryString;
        }
        return string;
    }

    clear() {
        this.query.clear();
    }

    addClause(clause) {
        if (clause instanceof API.Clause) {
            const uiClause = new Mapping.ClauseMapping(this.uiSegmentSrv).getUiClause(clause);
            this.query.addClause(uiClause);
        } else if (clause instanceof UI.Clause) {
            this.query.addClause(clause);
        } else {
            throw new Error("Clause type is not supported");
        }
    }

    withClause(clause) {
        this.addClause(clause);
        return this;
    }

    removeClause(clause) {
        this.query.removeClause(clause);
    }
}
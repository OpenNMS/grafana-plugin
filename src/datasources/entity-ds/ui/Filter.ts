import { API } from 'opennms';

import { Mapping } from '../Mapping';

import { Clause } from './Clause';
import { OrderBy } from './OrderBy';
import { Query } from './Query';

export class Filter {
    query: Query;

    constructor(public uiSegmentSrv: any, public entity: any) {
        this.query = new Query(uiSegmentSrv);
        this.query.root = true;
    }

    updateControls() {
        this.query.updateControls();
    }

    getQueryString() {
        const entityName = this.entity && this.entity.id ? this.entity.id : 'alarm';
        let string = 'select all ' + entityName + 's';
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

    addClause(clause: any) {
        if (clause instanceof API.Clause) {
            const uiClause = new Mapping.ClauseMapping(this.uiSegmentSrv, this.entity).getUiClause(clause);
            this.query.addClause(uiClause);
        } else if (clause instanceof Clause) {
            this.query.addClause(clause);
        } else {
            throw new Error("Clause type is not supported");
        }
    }

    withClause(clause: any) {
        this.addClause(clause);
        return this;
    }

    removeClause(clause: any) {
        this.query.removeClause(clause);
    }

    setOrder(order) {
        this.query.setOrder(order);
    }

    addOrderBy(orderBy: any) {
        if (orderBy instanceof API.OrderBy) {
            const uiOrderBy = new Mapping.OrderByMapping(this.uiSegmentSrv, this.entity).getUiOrderBy(orderBy);
            this.query.addOrderBy(uiOrderBy);
        } else if (orderBy instanceof OrderBy) {
            this.query.addOrderBy(orderBy);
        } else {
            throw new Error("OrderBy type is not supported");
        }
    }

    removeOrderBy(orderBy: OrderBy) {
        this.query.removeOrderBy(orderBy);
    }

    withOrderBy(orderBy: OrderBy) {
        this.addOrderBy(orderBy);
        return this;
    }
}

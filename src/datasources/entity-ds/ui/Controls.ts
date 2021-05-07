import _ from 'lodash';
import {Query} from './Query';


/**
 * Parent class of all controls.
 */
export abstract class Control {

    constructor(public title: string, public icon: string) {
    }

    abstract action(_query: any, _clause: any): void;

    abstract filter(_query: any, _clause: any): boolean;
}

/**
 * Control to remove the current clause from the current query.
 */
export class RemoveControl extends Control {
    constructor() {
        super("Remove clause", 'fa-minus');
    }

    action(query, clause) {
        query.removeClause(clause);
        if (query.root === false && query.getSize() === 0 && query.parentQuery !== undefined) {
            const parentClause = _.find(query.parentQuery.clauses, clause => {
                return clause.restriction === query
            });
            query.parentQuery.removeClause(parentClause);
        }
    }

    filter(query, clause) {
        if (clause.restriction instanceof Query) {
            return false;
        }
        if (query.root === true) {
            return query.clauses.length > 1 || clause.restriction.asRestrictionDTO() !== null;
        }
        return true;
    }
}

/**
 * Control to add a new clause to the current query.
 */
export class AddControl extends Control {

    constructor() {
        super('Add new clause', 'fa-plus');
    }

    action(query, clause) {
        const index = query.clauses.indexOf(clause) + 1;
        query.createNewEmptyClause(index);
    }

    filter(query, clause) {
        if (clause.restriction instanceof Query) {
            return false;
        }
        return true;
    }
}

/**
 * Control to add a new nested clause to the current query.
 */
export class AddNestedControl extends Control {
    constructor() {
        super('Add nested clause', 'fa-file', );
    }

    action(query, clause) {
        const index = query.clauses.indexOf(clause) + 1;
        query.createNewEmptyNestedClause(index);
    }

    filter(query, clause) {
        return query.root === true && !(clause.restriction instanceof Query);
    }
}

/**
 * Control to remove the current OrderBy from the query.
 */
export class RemoveOrderByControl extends Control {
    constructor() {
        super('Remove order by', 'fa-minus');
    }

    action(query, orderBy) {
        query.removeOrderByControl(orderBy);
    }

    filter(query /*, orderBy*/) {
        if (query.root === true) {
            return query.orderBy.length > 1;
        }
        return true;
    }
}

/**
 * Control to add a new OrderBy to the query.
 */
export class AddOrderByControl extends Control {

    constructor() {
        super('Add new order by', 'fa-plus');
    }

    action(query, orderBy) {
        const index = query.orderBy.indexOf(orderBy) + 1;
        query.createNewEmptyOrderBy(index);
    }

    filter(/*query, orderBy*/) {
        return true;
    }
}

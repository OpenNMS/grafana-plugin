import _ from 'lodash';
import {Restriction} from './restriction.js';
import {Query} from './query.js';


class Control {

    constructor(title, icon) {
        this.title = title;
        this.icon = icon;
    }

    action(query, clause) {
        throw new Error("Method action(...) not implemented");
    }

    filter(query, clause) {
        throw new Error("Method filter(...) not implemented");
    }
}

export class RemoveControl extends Control {
    constructor() {
        super("Remove clause", 'fa-minus');
    }

    action(query, clause) {
        query.removeClause(clause);
        if (query.root === false && query.getSize() === 0 && query.parentQuery !== undefined) {
            const parentClause = _.find(query.parentQuery.clauses, clause => {
                return clause.restriction == query
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

export class AddControl extends Control {

    constructor() {
        super('Add new clause', 'fa-plus');
    }

    action(query, clause) {
        query.createNewEmptyClause();
    }

    filter(query, clause) {
        if (clause.restriction instanceof Query) {
            return false;
        }
        return true;
    }
}

export class AddNestedControl extends Control {
    constructor() {
        super('Add nested clause', 'fa-file', );
    }

    action(query, clause) {
        query.createNewEmptyNestedClause();
    }

    filter(query, clause) {
        return query.root === true && !(clause.restriction instanceof Query);
    }
}
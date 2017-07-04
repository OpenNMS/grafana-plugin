import _ from 'lodash';
import {UI} from '../UI';

export class Query {

    constructor(uiSegmentSrv, parentQuery) {
        this.uiSegmentSrv = uiSegmentSrv;
        this.clauses = [];
        this.root = false;
        this.parentQuery = parentQuery;
    }

    clear() {
        this.clauses = [];
    }

    isEmpty() {
        return this.getSize() == 0;
    }

    getSize() {
        return this.clauses.length;
    }

    getLastClause() {
        if (this.clauses.length == 0) {
            return null;
        }
        return this.clauses[this.getSize() - 1];
    }

    updateControls() {
        // at least one row should be available even if it is a dummy row
        if (this.getSize() == 0) {
            this.createNewEmptyClause();
        }
        var self = this;
        _.each(this.clauses, clause => {
            clause.updateControls(self);
        });
    }

    addClause(clause, index) {
        if (clause) {
            if (!clause.uiSegmentSrv) {
                clause.uiSegmentSrv = this.uiSegmentSrv;
            }
            if (index !== undefined) {
                this.clauses.splice(index, 0, clause);
            } else {
                this.clauses.push(clause);
            }
        }
    }

    removeClause(clause) {
        if (clause) {
            var index = this.clauses.indexOf(clause);
            if (index >= 0) {
                this.clauses.splice(index, 1);
                return true;
            }
        }
        return false;
    }

    asString() {
        if (this.isEmpty()) {
            return "";
        }
        return _.map(this.clauses, function(clause, index) {
            let string = '';
            if (clause.restriction instanceof UI.Query) {
                const subString = clause.restriction.asString();
                if (subString && subString.length > 0) {
                    string += "(" + subString + ")";
                }
            } else if (clause.restriction) {
                const restrictionString = clause.restriction.asString();
                if (restrictionString && restrictionString.length > 0) {
                    string += restrictionString;
                }
            } else {
                throw {message: "Clause does not contain restriction. Bailing", clause: clause};
            }

            // Append operator if we have anything generated
            if (string.length > 0  && index > 0 && clause.operator && clause.operator.value) {
                string = " " + clause.operator.value.toLowerCase() + " " + string;
            }
            return string;
        }).join("");
    }

    createNewEmptyClause() {
        const newClause = new UI.Clause(this.uiSegmentSrv, UI.Operators.AND, new UI.Restriction(this.uiSegmentSrv));
        newClause.restriction.addSegment(this.uiSegmentSrv.newKey('select attribute'));
        newClause.restriction.addSegment(this.uiSegmentSrv.newOperator('='));
        newClause.restriction.addSegment(this.uiSegmentSrv.newFake('select value', 'value', 'query-segment-value'));
        this.addClause(newClause); // TODO MVR index?!
        return newClause;
    }

    createNewEmptyNestedClause() {
        const newQuery = new UI.Query(this.uiSegmentSrv, this);
        newQuery.createNewEmptyClause();
        const newClause = new UI.Clause(this.uiSegmentSrv, UI.Operators.AND, newQuery);
        this.addClause(newClause);
        return newQuery;
    }

    segmentUpdated(clause, segment) {
        // // If plus button was clicked, it is now an input field
        // if (segment.type === 'plus-button') {
        //     // make the plus button an actual attribute input
        //     segment.type = 'key';
        //     segment.cssClass = 'query-segment-key';
        //
        //     // If is the last element in the row, so move it to it's own row
        //     const segmentIndex = clause.restriction.segments.indexOf(segment);
        //     if (segmentIndex > 0) {
        //         const clauseIndex = this.clauses.indexOf(clause) + 1;
        //
        //         // remove plus button
        //         clause.restriction.removeLastSegment();
        //
        //         // Create new row
        //         clause = new UI.Clause(this.uiSegmentSrv, UI.Operators.AND, new UI.Restriction(this.uiSegmentSrv));
        //         this.addClause(clause, clauseIndex);
        //
        //         // add key (was plus button)
        //         clause.restriction.addSegment(segment);
        //     }
        //
        //     // Add comparator and value
        //     clause.restriction.addSegment(this.uiSegmentSrv.newOperator('='));
        //     clause.restriction.addSegment(this.uiSegmentSrv.newFake('select attribute value', 'value', 'query-segment-value'));
        // }

        if (segment.type === 'value') {
            segment.fake = false;
        }

        if (segment.type === 'condition') {
            clause.setOperator(segment.value);
        }

        // Ensure that we always have a plus button
        this.updateControls();
    }
}

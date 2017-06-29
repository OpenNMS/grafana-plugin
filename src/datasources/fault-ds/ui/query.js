import _ from 'lodash';
import {UI} from '../ui';

export class Query {

    constructor(uiSegmentSrv) {
        this.uiSegmentSrv = uiSegmentSrv;
        this.clauses = [];
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

    addPlusButtonIfRequired() {
        // at least one row should be available even if it is an empty row
        if (this.getSize() == 0) {
            this.clauses.push(new UI.Clause(this.uiSegmentSrv, UI.Operators.AND, new UI.Restriction(this.uiSegmentSrv)));
        }
        _.each(this.clauses, clause => {
            clause.addPlusButtonIfRequired();
        });
    }

    addClause(clause) {
        if (clause) {
            this.clauses.push(clause);
            if (!clause.uiSegmentSrv) {
                clause.uiSegmentSrv = this.uiSegmentSrv;
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

    segmentUpdated(clause, segment) {
        // If plus button was clicked, it is now an input field
        if (segment.type === 'plus-button') {
            // make the plus button an actual attribute input
            segment.type = 'key';
            segment.cssClass = 'query-segment-key';

            // It is the last element in the row, so move it to it's own row
            const segmentIndex = clause.restriction.segments.indexOf(segment);
            if (segmentIndex > 0) {
                // remove plus button
                clause.restriction.removeLastSegment();

                // Create new row
                clause = new UI.Clause(this.uiSegmentSrv, UI.Operators.AND, new UI.Restriction(this.uiSegmentSrv));
                this.addClause(clause);

                // add key (was plus button)
                clause.restriction.addSegment(segment);
            }

            // Add comparator and value
            clause.restriction.addSegment(this.uiSegmentSrv.newOperator('='));
            clause.restriction.addSegment(this.uiSegmentSrv.newFake('select attribute value', 'value', 'query-segment-value'));
        }

        if (segment.type === 'value') {
            segment.fake = false;
        }

        if (segment.type === 'condition') {
            clause.setOperator(segment.value);
        }

        // Ensure that we always have a plus button
        this.addPlusButtonIfRequired();
    }
}

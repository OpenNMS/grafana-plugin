'use strict';

System.register(['./ui/Filter', './ui/Query', './ui/Clause', './ui/Restriction', './ui/Operators', './ui/Comparators', './ui/Controls'], function (_export, _context) {
    "use strict";

    var Filter, Query, Clause, Restriction, RestrictionDTO, Operators, Comparators, AddControl, RemoveControl, AddNestedControl, Controls, UI;
    return {
        setters: [function (_uiFilter) {
            Filter = _uiFilter.Filter;
        }, function (_uiQuery) {
            Query = _uiQuery.Query;
        }, function (_uiClause) {
            Clause = _uiClause.Clause;
        }, function (_uiRestriction) {
            Restriction = _uiRestriction.Restriction;
            RestrictionDTO = _uiRestriction.RestrictionDTO;
        }, function (_uiOperators) {
            Operators = _uiOperators.Operators;
        }, function (_uiComparators) {
            Comparators = _uiComparators.Comparators;
        }, function (_uiControls) {
            AddControl = _uiControls.AddControl;
            RemoveControl = _uiControls.RemoveControl;
            AddNestedControl = _uiControls.AddNestedControl;
        }],
        execute: function () {
            _export('Controls', Controls = Object.freeze({
                AddControl: AddControl,
                AddNestedControl: AddNestedControl,
                RemoveControl: RemoveControl
            }));

            _export('Controls', Controls);

            _export('UI', UI = Object.freeze({
                Filter: Filter,
                Query: Query,
                Clause: Clause,
                Controls: Controls,
                Restriction: Restriction,
                RestrictionDTO: RestrictionDTO,
                Operators: Operators,
                Comparators: Comparators
            }));

            _export('UI', UI);
        }
    };
});
//# sourceMappingURL=UI.js.map

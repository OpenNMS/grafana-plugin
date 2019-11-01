'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.UI = exports.Controls = undefined;

var _Filter = require('./ui/Filter');

var _Query = require('./ui/Query');

var _Clause = require('./ui/Clause');

var _Restriction = require('./ui/Restriction');

var _Operators = require('./ui/Operators');

var _Comparators = require('./ui/Comparators');

var _Controls = require('./ui/Controls');

var Controls = exports.Controls = Object.freeze({
    AddControl: _Controls.AddControl,
    AddNestedControl: _Controls.AddNestedControl,
    RemoveControl: _Controls.RemoveControl
});

var UI = exports.UI = Object.freeze({
    Filter: _Filter.Filter,
    Query: _Query.Query,
    Clause: _Clause.Clause,
    Controls: Controls,
    Restriction: _Restriction.Restriction,
    RestrictionDTO: _Restriction.RestrictionDTO,
    Operators: _Operators.Operators,
    Comparators: _Comparators.Comparators
});
//# sourceMappingURL=UI.js.map

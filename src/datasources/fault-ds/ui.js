import {Filter} from './ui/filter.js';
import {Query} from './ui/query.js';
import {Clause} from './ui/clause.js';
import {Restriction, RestrictionDTO} from './ui/restriction.js';
import {Operators} from './ui/operator.js';
import {Comparators} from './ui/comparator.js';

import {AddControl} from './ui/Controls.js';
import {RemoveControl} from './ui/Controls.js';
import {AddNestedControl} from './ui/Controls.js';

export const Controls = Object.freeze({
    AddControl,
    AddNestedControl,
    RemoveControl
});

export const UI = Object.freeze({
   Filter,
    Query,
    Clause,
    Controls,
    Restriction,
    RestrictionDTO,
    Operators,
    Comparators,
});
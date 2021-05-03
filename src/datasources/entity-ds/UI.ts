import { Clause } from './ui/Clause';
import { Comparators } from './ui/Comparators';
import { Filter } from './ui/Filter';
import { Operators } from './ui/Operators';
import { OrderBy } from './ui/OrderBy';
import { Query } from './ui/Query';
import { Restriction, RestrictionDTO } from './ui/Restriction';

import { AddControl, RemoveControl, AddNestedControl, AddOrderByControl, RemoveOrderByControl } from './ui/Controls';

export const Controls = Object.freeze({
    AddControl,
    AddNestedControl,
    RemoveControl,
    AddOrderByControl,
    RemoveOrderByControl,
});

export const UI = Object.freeze({
    Filter,
    OrderBy,
    Query,
    Clause,
    Controls,
    Restriction,
    RestrictionDTO,
    Operators,
    Comparators,
});

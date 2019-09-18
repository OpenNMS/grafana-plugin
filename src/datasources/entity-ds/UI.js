import {Filter} from './ui/Filter';
import {Query} from './ui/Query';
import {Clause} from './ui/Clause';
import {Restriction, RestrictionDTO} from './ui/Restriction';
import {Operators} from './ui/Operators';
import {Comparators} from './ui/Comparators';
import {OrderBy} from './ui/OrderBy';

import {AddControl, RemoveControl, AddNestedControl, AddOrderByControl, RemoveOrderByControl} from './ui/Controls';

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
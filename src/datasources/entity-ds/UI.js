import {Filter} from './ui/Filter';
import {Query} from './ui/Query';
import {Clause} from './ui/Clause';
import {Restriction, RestrictionDTO} from './ui/Restriction';
import {Operators} from './ui/Operators';
import {Comparators} from './ui/Comparators';

import {AddControl} from './ui/Controls';
import {RemoveControl} from './ui/Controls';
import {AddNestedControl} from './ui/Controls';

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
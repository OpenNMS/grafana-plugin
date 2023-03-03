import {
    getFilterId,
    getFilterIdFromParts
} from '../datasources/entity-ds/EntityHelper'

import {
    ActiveFilter,
    FilterEditorData,
    FilterSelectableValues
} from '../datasources/entity-ds/types'

export const useFilterData = () => {
    return {
        getFilterId,
        getFilterIdFromParts
    }
}

export {
    ActiveFilter,
    FilterEditorData,
    FilterSelectableValues
}

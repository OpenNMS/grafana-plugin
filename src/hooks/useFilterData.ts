import {
    getFilterId,
    getFilterIdFromParts
} from '../datasources/entity-ds-react/EntityHelper'

import {
    ActiveFilter,
    FilterEditorData,
    FilterSelectableValues
} from '../datasources/entity-ds-react/types'

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

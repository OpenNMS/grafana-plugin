import {
    getFilterId,
    getFilterIdFromParts
} from '../datasources/entity-ds/EntityHelper'

export const useFilterData = () => {
    return {
        getFilterId,
        getFilterIdFromParts
    }
}

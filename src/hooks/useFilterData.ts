import {
    getFilterId,
    getFilterIdFromParts
} from '../datasources/entity-ds-react/EntityHelper'

export const useFilterData = () => {
    return {
        getFilterId,
        getFilterIdFromParts
    }
}

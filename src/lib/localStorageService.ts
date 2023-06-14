import { FilterEditorData, FilterEditorDataCollection } from '../datasources/entity-ds/types'

const FILTER_PANEL_STORAGE_KEY = 'opennms-filter-panel'

// save FilterEditorData for all dashboards, used internally
const saveAllFilterEditorData = (data: FilterEditorDataCollection) => {
  localStorage.setItem(FILTER_PANEL_STORAGE_KEY, JSON.stringify(data, getCircularReplacer()))
}

// load FilterEditorData for all dashboards, used internally
const loadAllFilterEditorData = (): FilterEditorDataCollection | null => {
    const json = localStorage.getItem(FILTER_PANEL_STORAGE_KEY)

    if (json) {
        const data = JSON.parse(json)
        
        if (data) {
          return data as FilterEditorDataCollection
        }
    }

    return null
}

// save filter editor data for a specific dashboard (data.dashboardUid contains id)
export const saveFilterEditorData = (data: FilterEditorData) => {
  if (!data || !data.dashboardUid) {
    throw new Error('Cannot save filter editor data: no dashboard uid.')
  }

  const existingData = loadAllFilterEditorData()
  const newData = { ...existingData, [data.dashboardUid]: data }
  localStorage.setItem(FILTER_PANEL_STORAGE_KEY, JSON.stringify(newData, getCircularReplacer()))
}

export const loadFilterEditorData = (dashboardUid: string): FilterEditorData | null => {
  if (!dashboardUid) {
    throw new Error('Cannot load filter editor data: no dashboard uid.')
  }

  const allData = loadAllFilterEditorData()

  if (allData) {
    const data = allData[dashboardUid]
    return data
  }

  return null
}

// remove filter data for the given dashboard
export const removeDashboardFilterEditorData = (dashboardUid: string) => {
  if (!dashboardUid) {
    throw new Error('Cannot remove filter editor data: no dashboard uid.')
  }

  const json = localStorage.getItem(FILTER_PANEL_STORAGE_KEY)

  if (json) {
      const data = JSON.parse(json) as FilterEditorDataCollection

      if (Object.keys(data).includes(dashboardUid)) {
        delete data[dashboardUid]
        saveAllFilterEditorData(data)
      }
  }
}

// clears all filter data for all dashboards
export const clearFilterEditorData = () => {
  localStorage.removeItem(FILTER_PANEL_STORAGE_KEY)
}

const getCircularReplacer = () => {
    const seen = new WeakSet()

    return (key, value) => {
        if (typeof value === 'object' && value !== null) {
            if (seen.has(value)) {
                return
            }
            seen.add(value)
        }
        return value
    }
}

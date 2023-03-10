import React, { useEffect, useState } from 'react'
import { MetricFindValue, PanelProps, SelectableValue } from '@grafana/data'
import { getDataSourceSrv } from "@grafana/runtime"
import { HorizontalGroup, Input, Select, VerticalGroup } from '@grafana/ui'
import { FieldDisplay } from 'components/FieldDisplay'
import { ALL_SELECTION_VALUE } from 'constants/constants'
import { loadFilterEditorData, saveFilterEditorData } from 'lib/localStorageService'
import { FilterControlProps } from './FilterPanelTypes'
import { useEntities } from '../../hooks/useEntities'
import { useFilterData } from '../../hooks/useFilterData'
import { ActiveFilter, FilterEditorData, FilterSelectableValues } from '../../datasources/entity-ds-react/types'

// Values actually returned by OpenNMS metricFindQuery
interface OnmsMetricFindValue extends MetricFindValue {
    id?: string;
    label?: string;
}

export const FilterPanelControl: React.FC<PanelProps<FilterControlProps>> = (props) => {
    const { getFuncNameFromEntityType } = useEntities()
    const { getFilterId } = useFilterData()

    // array of metric values for each filter id
    const [metricValues, setMetricValues] = useState<Array<[string, OnmsMetricFindValue[]]>>([])

    // array of selectable values for each filter id
    const [selectableValues, setSelectableValues] = useState<FilterSelectableValues[]>([])

    useEffect(() => {
        const filterEditorData = loadFilterEditorData()

        if (filterEditorData && filterEditorData.selectableValues) {
            setSelectableValues(filterEditorData.selectableValues)
        }
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    useEffect(() => {
        const fetchMetricValues = async () => {
            const newMetricValues: Array<[string, OnmsMetricFindValue[]]> = []
            let shouldUpdate = false

            // loop through filters, create metricValues entry for each
            for (const filter of props.options.filterEditor.activeFilters || []) {
                const filterId = getFilterId(filter)
                const [key, values] = metricValues.find(([k,v]) => k === filterId) || []

                if (key) {
                    // key already existed, assume values were already fetched
                    newMetricValues.push([key, values || []])
                } else {
                    // values were not previously retrieved
                    const newValues = await getValuesFromDatasource(filter)
                    newMetricValues.push([filterId, newValues])
                    shouldUpdate = true
                }
            }

            if (shouldUpdate) {
                setMetricValues(newMetricValues)
            }
        }

        fetchMetricValues()

        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [props])

    const getValuesFromDatasource = async (filter: ActiveFilter) => {
        const grafanaDatasource = props.options.filterEditor?.datasource?.value

        if (grafanaDatasource && grafanaDatasource?.id) {
            const datasourceSrv = getDataSourceSrv()
            const qualifiedDatasource = await datasourceSrv.get(grafanaDatasource)

            if (qualifiedDatasource && qualifiedDatasource.metricFindQuery) {
                const entityFunc = getFuncNameFromEntityType(filter.entity.label ?? '')
                const entity = (filter.entity.label || '').toLowerCase()
                const attr = filter.attribute.id

                if (entity && attr) {
                    const opts = { entityType: entity }
                    const query = `${entityFunc}(${attr})`

                    const metricFindValues = await qualifiedDatasource.metricFindQuery(query, opts)

                    // got values from the datasource, store them and set update flag
                    if (metricFindValues.length > 0) {
                        const onmsMetricFindValues = metricFindValues.map(v => {
                            return {
                                id: v['id'] ? v['id'] : '',
                                label: v['label'] ? v['label'] : '',
                                text: v.text,
                                value: v.value,
                            } as OnmsMetricFindValue
                        })

                        return onmsMetricFindValues
                    }
                }
            }
        }

        return []
    }

    const generateFilterSelectOptions = (filter: ActiveFilter) => {
        const filterId = getFilterId(filter)
        const isMulti = filter.selectionType?.label === 'Multi'
        const [key, currentValues] = metricValues.find(([k,v]) => k === filterId) || []

        if (key && currentValues) {
            const values = currentValues.map(v => ({
                label: v.text,
                value: v.value || ''
            } as SelectableValue<string | number>))

            // Single select dropdown add 'All'
            // For multi-select, user would just not select anything
            if (!isMulti) {
                const allValue = { label: 'All', value: ALL_SELECTION_VALUE}
                return [allValue, ...values]
            }

            return values
        }

        return []
    }

    const filterDisplayLabel = (filter: ActiveFilter, index: number) => {
        const altLabel = filter.altColumnLabel

        if (altLabel) {
            return altLabel
        }

        return `${filter.entity.label || ''}: ${filter.attribute.label || ''}`
    }

    const getInputSelectableValue = (filter: ActiveFilter) => {
        const filterId = getFilterId(filter)
        const selVals = selectableValues.find(v => v.filterId === filterId)

        if (selVals && selVals.values?.length > 0) {
            return selVals.values[0].value || ''
        }

        return ''
    }

    const getSelectSelectableValues = (filter: ActiveFilter) => {
        const filterId = getFilterId(filter)
        const selVals = selectableValues.find(v => v.filterId === filterId)

        return (selVals && selVals.values) || []
    }

    const inputChanged = (e, filter: ActiveFilter) => {
        const filterId = getFilterId(filter)
        const newValues = selectableValues.filter(v => v.filterId !== filterId)

        const newValue = {
            label: '',
            value: (e.target.value || '') as string
        }

        const newEntry = {
            filterId,
            values: [newValue]
        } as FilterSelectableValues

        newValues.push(newEntry)
        setSelectableValues(newValues)
        saveFilterPanel(newValues)
    }

    const selectChanged = (value: (SelectableValue | SelectableValue[]), filter: ActiveFilter) => {
        const filterId = getFilterId(filter)
        const newValue = Array.isArray(value) ? value : [value]
        const newValues = selectableValues.filter(v => v.filterId !== filterId)

        const newEntry = {
            filterId,
            values: newValue
        } as FilterSelectableValues

        newValues.push(newEntry)
        setSelectableValues(newValues)
        saveFilterPanel(newValues)
    }

    /**
     * Save filter panel data to localStorage, for use by Entity Datasource when filtering queries.
     */
    const saveFilterPanel = (filterSelectableValues: FilterSelectableValues[]) => {
        if (props.options.filterEditor) {
            // TODO: May want to remove some data from datasource before saving
            const filterData: FilterEditorData = {
                ...props.options.filterEditor,
                selectableValues: filterSelectableValues
            }

            saveFilterEditorData(filterData)
        }
    }

    useEffect(() => {
        saveFilterPanel(selectableValues)
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [props.options.filterEditor])

    return (
        <div style={{ height: '100%', overflowY: 'auto' }}>
            <VerticalGroup align='flex-start'>
                {props.options?.filterEditor?.activeFilters?.map &&
                 props.options?.filterEditor?.activeFilters.map((filter, index) => { 
                    return (
                        <HorizontalGroup key={getFilterId(filter)}>
                            <FieldDisplay>{filterDisplayLabel(filter, index)}</FieldDisplay>
                            {filter.selectionType?.label === 'Text' ?
                                <Input
                                   value={getInputSelectableValue(filter)} 
                                   onChange={(e) => inputChanged(e, filter)}
                                />
                                : <Select
                                    isMulti={filter.selectionType?.label === 'Multi'}
                                    options={generateFilterSelectOptions(filter)}
                                    value={getSelectSelectableValues(filter)}
                                    menuShouldPortal={true}
                                    onChange={(value) => selectChanged(value, filter)}></Select>
                            }
                        </HorizontalGroup>
                    )
                })}
            </VerticalGroup>
        </div>
    )
}

import React, { useState, useEffect } from 'react'
import { PanelOptionsEditorProps, SelectableValue } from '@grafana/data'
import { GrafanaDatasource } from 'hooks/useDataSources'
import { useOpenNMSClient } from '../../hooks/useOpenNMSClient'
import { ActiveFilter } from '../../datasources/entity-ds/types'
import { FilterPanelDataSource } from './FilterPanelDataSource'
import { FilterPanelLayoutOptions } from './FilterPanelLayoutOptions'
import { FilterPanelFilterSelector } from './FilterPanelFilterSelector'
import { FilterPanelActiveFilters } from './FilterPanelActiveFilters'
import { loadFilterEditorData } from 'lib/localStorageService'
import { ClearFilterData } from '../../components/ClearFilterData'

interface FilterPanelOptionOptions {
    datasource: SelectableValue<GrafanaDatasource>
    activeFilters: ActiveFilter[]
    isHorizontalLayout: boolean
}

export const FilterPanelOptions: React.FC<PanelOptionsEditorProps<FilterPanelOptionOptions>> = (props) => {
    const [internalOptions, setInternalOptions] = useState<FilterPanelOptionOptions>(
        {
            datasource: props.context.options?.datasource || {},
            activeFilters: props.context.options?.activeFilters || [],
            isHorizontalLayout: props.context.options?.isHorizontalLayout || false
        })
    const { client } = useOpenNMSClient(internalOptions?.datasource?.value)
 
    const onOptionChange = (v, k) => {
        setInternalOptions((oldOptions) => {
            const newOptions = { ...oldOptions }
            newOptions[k] = v
            return newOptions
        })
    }

    useEffect(() => {
        const dashboardUid = props.context.options?.dashboardUid || ''

        if (dashboardUid) {
          const data = loadFilterEditorData(dashboardUid)

          if (data && data.datasource && data.activeFilters) {
              setInternalOptions({
                  datasource: data.datasource,
                  activeFilters: data.activeFilters,
                  isHorizontalLayout: data.isHorizontalLayout
              })
          }
        }
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    useEffect(() => {
        props.onChange({
            ...internalOptions
        })
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [internalOptions])

    return (
        <>
            <style>
            {
                `
                .spacer {
                    margin-top: 10px;
                    margin-bottom: 10px;
                }
              `
            }
            </style>
            <FilterPanelLayoutOptions
                isHorizontal={internalOptions.isHorizontalLayout}
                onChange={(d) => onOptionChange(d, 'isHorizontalLayout')}
            />
            <FilterPanelDataSource
                onChange={(d) => onOptionChange(d, 'datasource')}
                datasource={internalOptions.datasource}
            />
            <FilterPanelFilterSelector
                activeFilters={internalOptions.activeFilters}
                client={client}
                datasource={internalOptions.datasource.value}
                onChange={(d) => onOptionChange(d, 'activeFilters')}
            />
            <FilterPanelActiveFilters
                activeFilters={internalOptions.activeFilters}
                onChange={(d) => onOptionChange(d, 'activeFilters')}
            />

            <ClearFilterData />
        </>
    )
}

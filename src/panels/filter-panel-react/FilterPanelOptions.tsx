import React, { useState, useEffect } from 'react'
import { PanelOptionsEditorProps, SelectableValue } from '@grafana/data'
import { GrafanaDatasource } from 'hooks/useDataSources';
import { useOpenNMSClient } from '../../hooks/useOpenNMSClient'
import { ActiveFilter } from '../../datasources/entity-ds-react/types'
import { FilterPanelDataSource } from './FilterPanelDataSource'
import { FilterPanelFilterSelector } from './FilterPanelFilterSelector';
import { FilterPanelActiveFilters } from './FilterPanelActiveFilters';
import { loadFilterEditorData } from 'lib/localStorageService'

interface FilterPanelOptionOptions {
    datasource: SelectableValue<GrafanaDatasource>
    activeFilters: ActiveFilter[]
}

export const FilterPanelOptions: React.FC<PanelOptionsEditorProps<FilterPanelOptionOptions>> = (props) => {
    const [internalOptions, setInternalOptions] = useState<FilterPanelOptionOptions>(
        {
            datasource: props.context.options?.datasource || {},
            activeFilters: props.context.options?.activeFilters || []
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
        const data = loadFilterEditorData()

        if (data && data.datasource && data.activeFilters) {
            setInternalOptions({
                datasource: data.datasource,
                activeFilters: data.activeFilters
            })
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
        </>
    )
}

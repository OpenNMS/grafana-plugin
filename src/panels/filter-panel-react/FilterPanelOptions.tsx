import { PanelOptionsEditorProps, SelectableValue } from '@grafana/data'
import { GrafanaDatasource } from 'hooks/useDataSources';
import { useOpenNMSClient } from '../../hooks/useOpenNMSClient'
import React, { useState, useEffect } from 'react'
import { ActiveFilter } from './FilterPanelTypes';
import { FilterPanelDataSource } from './FilterPanelDataSource'
import { FilterPanelFilterSelector } from './FilterPanelFilterSelector';
import { FilterPanelActiveFilters } from './FilterPanelActiveFilters';

interface FilterPanelOptionOptions {
    datasource: SelectableValue<GrafanaDatasource>
    activeFilters: ActiveFilter[]
}
export const FilterPanelOptions: React.FC<PanelOptionsEditorProps<{}>> = (props) => {
    const [activeFilters, setActiveFilters] = useState<ActiveFilter[]>([])
    const [internalOptions, setInternalOptions] = useState<FilterPanelOptionOptions>({ datasource: {}, activeFilters: [] })
    const { client } = useOpenNMSClient(internalOptions?.datasource?.value)
 
    useEffect(() => {
        if (activeFilters){
            onOptionChange(activeFilters,'activeFilters')
        }
    },[activeFilters])

    const onOptionChange = (v, k) => {

        setInternalOptions((oldOptions) => {
            const newOptions = { ...oldOptions }
            newOptions[k] = v
            return newOptions
        })
    }

    useEffect(() => {
        props.onChange({
            ...internalOptions
        })
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [internalOptions])

    /*
        TODO: When the entity datasource metric find query is working better, retry this stuff below.
    const getPropertiesFromDatasource = async () => {
         
          const datasourceSrv = getDataSourceSrv()
          const qualifiedDatasource = await datasourceSrv.get(datasource?.value)
          const opts = {
              queryType: 'attributes',
          };
          if (entity) {
              opts['entityType'] = entity.id;
          }
          if (qualifiedDatasource && qualifiedDatasource.metricFindQuery) {
              const activeProperties = await qualifiedDatasource.metricFindQuery(entity, opts)
              // TODO: When this return active values, store them and show them in the Filter Control
          } 
          
    }
    TODO: See above why this is temporarily disabled.
    useEffect(() => {
        getPropertiesFromDatasource();
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [datasource])
    */


    return (
        <>
            <FilterPanelDataSource onChange={(d) => onOptionChange(d, 'datasource')} />
            <FilterPanelFilterSelector
                activeFilters={activeFilters}
                setActiveFilters={setActiveFilters}
                client={client}
                datasource={internalOptions?.datasource.value}
                onChange={(d) => onOptionChange(d, 'activeFilters')}
            />
            <FilterPanelActiveFilters
                activeFilters={internalOptions?.activeFilters || []}
                setActiveFilters={setActiveFilters}
                onChange={(d) => setInternalOptions((oldOptions) => ({ ...oldOptions, ...d }))}
            />

        </>
    )
}

import React, { useState } from 'react';
import { Button, HorizontalGroup, Label, Select } from '@grafana/ui'
import { GrafanaDatasource } from 'hooks/useDataSources';
import { ClientDelegate } from 'lib/client_delegate';
import { useEntityProperties } from 'hooks/useEntityProperties';
import { ActiveFilter } from './FilterPanelTypes';
import { SelectableValue } from '@grafana/data';
import { useEntities } from 'hooks/useEntities';

interface FilterPanelFilterSelectorProps {
    datasource?: GrafanaDatasource,
    onChange: Function,
    client?: ClientDelegate,
    activeFilters: ActiveFilter[]
    setActiveFilters: Function
}

export const FilterPanelFilterSelector: React.FC<FilterPanelFilterSelectorProps> = ({ datasource, onChange, client,activeFilters,setActiveFilters }) => {
    const entityOptions = useEntities();
    const [entity, setEntity] = useState<SelectableValue<string>>()
    const [attribute, setAttribute] = useState<SelectableValue<{ id: string }>>()
    const { propertiesAsArray } = useEntityProperties(entity?.label || '', false, client as any)
    console.log('HI!',entity?.label,propertiesAsArray)
    const disabled = !entity || !attribute || !datasource || attribute.label === 'Select Attribute';
    const addFilterRow = () => {
        setActiveFilters((oldFilters) => {
            let newFilters: any = []
            if (oldFilters) {
                newFilters = [...oldFilters]
                newFilters.push({ entity, attribute })
            }
            return newFilters;
        })
        setAttribute({ label: 'Select Attribute' })
    }
   
    return (
        <>
            <Label style={{ marginTop: 12 }}>
                Filters
            </Label>
            <HorizontalGroup>
                <Select placeholder='Entity' options={entityOptions} onChange={(e) => setEntity(e)} value={entity} />
                <Select placeholder='Attribute' options={propertiesAsArray} onChange={(e) => setAttribute(e)} value={attribute} />
                <Button disabled={disabled} onClick={addFilterRow}>Add Filter Row</Button>
            </HorizontalGroup>
        </>
    )
}

import React, { useState } from 'react'
import { SelectableValue } from '@grafana/data'
import {
    Button,
    HorizontalGroup,
    InlineField,
    InlineFieldRow,
    Label,
    Select,
    Switch
} from '@grafana/ui'
import { GrafanaDatasource } from 'hooks/useDataSources'
import { useEntities } from 'hooks/useEntities'
import { useEntityProperties } from 'hooks/useEntityProperties'
import { ClientDelegate } from 'lib/client_delegate'
import { getFilterId, getFilterIdFromParts } from './FilterPanelHelper'
import { ActiveFilter } from './FilterPanelTypes'

interface FilterPanelFilterSelectorProps {
    datasource?: GrafanaDatasource,
    activeFilters: ActiveFilter[],
    client?: ClientDelegate,
    onChange: Function
}

export const FilterPanelFilterSelector: React.FC<FilterPanelFilterSelectorProps> =
    ({ datasource, activeFilters, client, onChange }) => {

    const { entities: entityOptions } = useEntities();
    const [entity, setEntity] = useState<SelectableValue<string>>()
    const [attribute, setAttribute] = useState<SelectableValue<{ id: string }>>()
    const [featuredAttributes, setFeaturedAttributes] = useState<boolean>(true)
    const { propertiesAsArray } = useEntityProperties(entity?.label || '', featuredAttributes, client as any)

    const isDisabled = () => {
        if (!entity || !attribute || !datasource || attribute.label === 'Select Attribute') {
            return true
        }

        // prevent adding duplicate filters
        const filterId = getFilterIdFromParts(entity, attribute)
        return activeFilters.some(f => getFilterId(f) === filterId)
    }

    /**
     * Values in propertiesAsArray contains additional properties such as 'valueProvider'
     * which are very large and cause circular references (causing issues with JSON.stringify)
     * and are not needed. We just return the necessary properties here
     */
    const getAttributeOptions = () => {
        return propertiesAsArray.map(p => ({
            id: p.id,
            label: p.label,
            name: p.name,
            orderBy: p.orderBy,
            value: p.value,
            type: p.type
        }))
    }

    const addFilterRow = () => {
        let newFilters: any = []
        if (activeFilters) {
            newFilters = [...activeFilters]
            newFilters.push({ entity, attribute })
        }

        onChange(newFilters)
        setAttribute({ label: 'Select Attribute' })
    }

    return (
        <>
            <style>
                {`
                    .spacer {
                        margin-bottom: 6px;
                    }
                `}
            </style>
             <Label style={{ marginTop: 12 }}>
                Filters
            </Label>
            <HorizontalGroup>
                <Select placeholder='Entity' options={entityOptions} onChange={(e) => setEntity(e)} value={entity} />
                <Select placeholder='Attribute' options={getAttributeOptions()} onChange={(e) => setAttribute(e)} value={attribute} />
                <Button disabled={isDisabled()} onClick={addFilterRow}>Add Filter Row</Button>
            </HorizontalGroup>
            <div className='spacer' />
            <InlineFieldRow>
                <InlineField label='Featured attributes'>
                    <div style={{ display: 'flex', alignItems: 'center', height: '32px' }}>
                        <Switch
                            value={featuredAttributes}
                            onChange={() => setFeaturedAttributes(!featuredAttributes)} />
                    </div>
                </InlineField>
            </InlineFieldRow>
        </>
    )
}

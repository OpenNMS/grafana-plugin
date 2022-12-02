import { SelectableValue } from '@grafana/data'
import { getDataSourceSrv } from '@grafana/runtime';
import { Badge, Button, IconButton, InlineField, InlineFieldRow, Select } from '@grafana/ui'
import { FieldDisplay } from 'components/FieldDisplay';
import { useDatasources } from 'hooks/useDataSources';
import { useEntities } from 'hooks/useEntities';
import React, { useState, useEffect } from 'react'

interface ActiveFilter {
    attribute: SelectableValue<string | number>,
    entity: SelectableValue<string | number>,
}
export const FilterEditor = (props) => {

    const [activeFilters, setActiveFilters] = useState<ActiveFilter[]>([])
    const [entity, setEntity] = useState<SelectableValue<string | number>>()
    const [datasource, setDatasource] = useState<SelectableValue<string>>()
    const [attribute, setAttribute] = useState<SelectableValue<string | number>>()
    const [attributes, setAttributes] = useState<Array<SelectableValue<string>>>([])

    const { datasources } = useDatasources();
    const allowedDatasources = ['opennms-helm-entity-datasource-react', 'opennms-helm-entity-datasource']
    const entityDatasources = datasources?.filter((d) => allowedDatasources.includes(d.type))
    const selectOptions = entityDatasources?.map((d) => ({ label: d.name, value: d.id, ...d }))
    const entities = useEntities();

    const updateDatasource = async () => {
        const b = getDataSourceSrv()
        const x = await b.get(datasource?.value)
        const opts = {
            queryType: 'attributes',
        };
        if (entity) {
            opts['entityType'] = entity.id;
        }
        if (x && x.metricFindQuery) {
            const bb = await x.metricFindQuery(entity, opts)
            const mapped = bb.map((b: any) => ({ label: b.name, value: b.id, ...b }))
            setAttributes(mapped);
        }
    }
    useEffect(() => {
        updateDatasource();
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [datasource])
    const addFilterRow = () => {
        setActiveFilters((oldFilters) => {
            let newFilters: any = []
            if (oldFilters) {
                newFilters = [...oldFilters]
                newFilters.push({ entity, attribute })
            }
            return newFilters;
        })
    }
    const disabled = !entity || !attribute || !datasource;
    return (
        <>
            <InlineField label='Data source entity'>
                <div style={{ display: 'flex', gap: '10px' }}>
                    <Select options={selectOptions} onChange={(e) => setDatasource(e)} value={datasource} />
                    <Select options={entities} onChange={(e) => setEntity(e)} value={entity} />
                    <Select options={attributes} onChange={(e) => setAttribute(e)} value={attribute} />
                    <Button disabled={disabled} onClick={addFilterRow}>Add Filter Row</Button>
                </div>
            </InlineField>
            <style>
                {
                    `
                    .no-margin {
                        margin:0;
                    }
                    `
                }
            </style>
            {activeFilters.length > 0 &&
                <div style={{marginBottom:'6px'}}>

                    <div style={{ marginTop: '20px;' }}>&nbsp;</div>
                    <Badge text='Active Filters' color='blue'></Badge>
                    <div style={{ marginTop: '6px;' }}>&nbsp;</div>
                    {activeFilters?.map((filter, index) => {
                        return (
                            <InlineFieldRow key={index}>
                                <InlineField label="Entity" className='no-margin'>
                                    <FieldDisplay> {filter?.entity?.label}</FieldDisplay>
                                </InlineField>
                                <InlineField label="Attribute" className='no-margin'>
                                    <FieldDisplay>{filter?.attribute?.name}</FieldDisplay>
                                </InlineField>
                                <FieldDisplay icon color='#AA0000'>
                                    <IconButton key='remove' name='trash-alt' tooltip='Remove Filter' />
                                </FieldDisplay>
                            </InlineFieldRow>
                        )
                    })}
                </div>
            }
        </>
    )
}

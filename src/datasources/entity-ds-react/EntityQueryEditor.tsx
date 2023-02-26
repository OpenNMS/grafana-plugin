import React, { useState, useEffect } from 'react'
import {
    InlineField,
    InlineFieldRow,
    Input,
    Segment,
    SegmentSection,
    Spinner,
    Switch,
} from '@grafana/ui'
import { SelectableValue } from '@grafana/data';
import { useEntityProperties } from '../../hooks/useEntityProperties';
import { EntityClauseEditor } from './EntityClauseEditor';
import { EntityOrderByEditor } from './EntityOrderByEditor';
import { defaultClause } from './constants';
import { EntityQueryEditorProps } from './types';
import { getSmallerAPIFilter } from './EntityHelper';

export const EntityQueryEditor: React.FC<EntityQueryEditorProps> = ({ onChange, query, onRunQuery, datasource, ...rest }) => {

    const client = datasource.client;
    const [value, setValue] = useState<SelectableValue<string>>(query.selectType || { label: 'Alarms' })
    const [clauses, setClauses] = useState<any>(query.clauses || [{ ...defaultClause }])
    const [loading, setLoading] = useState(false)
    const [filter, setFilter] = useState(query?.filter || getSmallerAPIFilter())
    const [limit, setLimit] = useState(query?.filter?.limit || 99)
    const [featuredAttributes, setFeaturedAttributes] = useState(true)
    const { propertiesLoading, propertiesAsArray } = useEntityProperties(value.label || '', featuredAttributes, client)

    useEffect(() => {
        if (propertiesLoading) {
            setLoading(true)
        } else {
            setLoading(false);
        }
    }, [propertiesLoading, setLoading])

    useEffect(() => {
        updateQuery()
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [filter])

    useEffect(() => {
        setClauses([{ ...defaultClause }])
        
    }, [value])

    const updateQuery = () => {
        onChange({
            ...query,
            selectType: value,
            filter,
            clauses
        })

        onRunQuery()
    }

    const updateLimit = (newLimit) => {
        setLimit(newLimit)
        setFilter({
            ...filter,
            limit: newLimit
        })
    }

    return (<div className='bigger-labels'>
        <style>
            {`
                .bigger-labels label {
                    min-width: 32px;
                }
                .spacer {
                    margin-bottom: 6px;
                }
                .max-input {
                    max-width: 150px;
                }
            `}
        </style>
        <SegmentSection label='Select'>
            <Segment
                value={value}
                onChange={(item) => {
                    setValue(item);
                }}
                allowEmptyValue={false}
                options={[
                    { label: 'Alarms' },
                    { label: 'Nodes' },
                    { label: 'IP Interfaces' },
                    { label: 'SNMP Interfaces' },
                    { label: 'Monitored Services' },
                    { label: 'Outages' },
                ]}
            />
            {loading && <div style={{ display: 'flex', alignItems: 'center' }}>
                <Spinner />
            </div>}
        </SegmentSection>
        <div className='spacer' />

        <EntityClauseEditor
            clauses={clauses}
            setClauses={setClauses}
            setFilter={setFilter}
            loading={loading}
            propertiesAsArray={propertiesAsArray}
        />

        <EntityOrderByEditor
            filter={filter}
            setFilter={setFilter}
            searchAttributes={propertiesAsArray}
        />

        <div className='spacer' />
        <InlineFieldRow>
            <InlineField label='Limit'>
                <Input className='max-input' type='number' value={limit}
                    onChange={(ev) => {
                        const elem = (ev.target as HTMLInputElement)
                        if (elem) {
                            updateLimit(Number(elem.value))
                        }
                    }} />
            </InlineField>
        </InlineFieldRow>
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
    </div>)
}

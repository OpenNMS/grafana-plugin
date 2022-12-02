import React, { useState, useEffect } from 'react'
import { Segment, SegmentSection, Spinner, Button, InlineField, InlineFieldRow, Input, Switch } from '@grafana/ui'
import { SelectableValue } from '@grafana/data';

import { useEntityProperties } from './useEntityProperties';
import { EntityClauseEditor } from './EntityClauseEditor';
import { defaultClause, defaultOrder } from './constants';
import { EntityQueryEditorProps } from './types';
import { getSmallerAPIFilter } from './EntityHelper';


export const EntityQueryEditor: React.FC<EntityQueryEditorProps> = ({ onChange, query, onRunQuery, datasource, ...rest }) => {

    const client = datasource.client;
    const [value, setValue] = useState<SelectableValue<string>>(query.selectType || { label: 'Alarms' });
    const [clauses, setClauses] = useState<any>(query.clauses || [{ ...defaultClause }])
    const [loading, setLoading] = useState(false);
    const [filter, setFilter] = useState(query.filter || getSmallerAPIFilter());
    const [orderedValue, setOrderedValue] = useState<SelectableValue<string>>(defaultOrder);
    const [limit, setLimit] = useState(100);
    const [featuredAttributes, setFeaturedAttributes] = useState(true);

    const { propertiesLoading, propertiesAsArray } = useEntityProperties(value.label || '', featuredAttributes, client);

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
    }, [filter, clauses, value]);

    const updateQuery = () => {
        onChange({
            ...query,
            selectType: value,
            filter,
            clauses
        })

        onRunQuery();
    }

    const addOrderBy = () => {
        //TODO: This logic.
    }

    const clearRestrictionsAndOrderBy = () => {
        setClauses([{ ...defaultClause }])
        setOrderedValue(defaultOrder)
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
                onChange={(text) => {
                    clearRestrictionsAndOrderBy();
                    setValue(text);
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
            filter={filter}
            setFilter={setFilter}
            loading={loading}
            propertiesAsArray={propertiesAsArray}
        />

        <SegmentSection label='ORDER BY'>
            <Segment
                allowEmptyValue={false}
                value={orderedValue}
                onChange={(text) => {
                    setOrderedValue(text);
                }}
                options={[{ label: 'DESC' }, { label: 'ASC' }]}
            />
            <Button onClick={() => addOrderBy()} size='xs'>+</Button>
        </SegmentSection>
        <div className='spacer' />
        <InlineFieldRow>
            <InlineField label='Limit'>
                <Input className='max-input' type='number' value={limit} onChange={(b) => setLimit(Number(b.currentTarget.value))} />
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

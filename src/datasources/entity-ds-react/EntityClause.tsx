import { SelectableValue } from '@grafana/data';
import { Segment, SegmentInput, Spinner, Button, InlineFieldRow } from '@grafana/ui';
import React, { useEffect, useState } from 'react'
import { EntityClauseLabel } from './EntityClauseLabel';
import { Comparator, EntityClauseProps, OnmsEntityType, OnmsEntityNestType, SearchOption } from './types';
import { API } from 'opennms'

export const EntityClause = ({
    clause,
    addClause,
    addNestedClause,
    addSubClause,
    removeClause,
    propertiesAsArray,
    setAttribute,
    setComparator,
    setComparedValue,
    setComparedString,
    setClauseType,
    loading,
    index,
    hasMultipleClauses
}: EntityClauseProps) => {
    const [comparatorOptions, setComparatorOptions] = useState<Array<SelectableValue<Comparator>>>([]);
    const [comparedOptions, setComparedOptions] = useState<Array<SelectableValue<string>>>([]);

    useEffect(() => {
        const { values, comparators } = getValuesAndComparatorsFromClause();

        if (values) {
            buildOptionsFromAttributeValues(values);
        }

        if (comparators) {
            setComparatorOptions(comparators)
            setComparator(index, comparators[0])
        }

        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [clause.attribute])

    const buildOptionsFromAttributeValues = (values: string[]) => {
        values = { ...values };
        const options = Object.keys(values).map((i) => {
            const val = values?.[i] || ''
            return { value: val, label: val }
        })
        setComparedOptions(options)
    }

    const getValuesAndComparatorsFromClause = () => {
        let values = clause.attribute?.value?.values;
        let comparators = clause.attribute?.value?.type?.comparators?.map((c) => (
            {
                label: c.l,
                value: c
            }
        ))
        if (values && !comparators) {
            const comp = API.Comparators.EQ
            comparators = [{ label: comp.label, value: comp }]
        }
        return { values, comparators }
    }

    const getInputTypeFromAttributeType = (attribute: SearchOption | undefined) => {
        let type = 'text';
        if (attribute?.value?.type?.i === 'TIMESTAMP') {
            type = 'date'
        } else if (attribute?.value?.type?.i === 'INTEGER') {
            type = 'number'
        }
        return type;
    }

    const resetOrRemoveClause = (col: number) => {
        if (clause.type !== OnmsEntityType.FIRST || hasMultipleClauses) {
            removeClause(col)
        }
        else {
            setAttribute(col, {})
            setComparator(col, {})
            setComparedString(col, '')
            setComparedValue(col, {})
        }
    }

    return (
        <InlineFieldRow>
            <EntityClauseLabel
                type={clause.type}
                nestingType={clause.nestingType}
                index={index}
                setClauseType={setClauseType}
            />
            <Segment
                placeholder='select attribute'
                value={clause.attribute}
                onChange={(text) => {
                    setAttribute(index, text);
                }}
                allowEmptyValue={false}
                options={propertiesAsArray.map((prop) => {
                    return {
                        label: prop.label,
                        type: prop.type,
                        value: {
                            id: prop.id,
                            label: prop.label,
                            name: prop.name,
                            orderBy: prop.orderBy,
                            type: prop.type,
                            values: prop.values
                        }
                    }
                })}
            />
            <Segment
                allowEmptyValue={false}
                value={clause.comparator}
                onChange={(text) => {
                    setComparator(index, text);
                }}
                options={comparatorOptions}

            />
            {clause.attribute?.value?.values && Object.keys(clause.attribute?.value.values).length > 0 ?
                <Segment
                    allowEmptyValue={false}
                    value={clause.comparedValue}
                    onChange={(text) => {
                        setComparedValue(index, text);
                    }}
                    options={comparedOptions}
                /> :
                <SegmentInput
                    placeholder='select value'
                    type={getInputTypeFromAttributeType(clause.attribute)}
                    onChange={(text) => {
                        setComparedString(index, text);
                    }}
                    value={clause.comparedString}
                />
            }
            {loading && <div style={{ display: 'flex', alignItems: 'center' }}>
                <Spinner />
            </div>
            }
            {clause.nestingType === OnmsEntityNestType.TOP ? <>
                <Button onClick={() => addClause(index)} size='xs' style={{ marginRight: '5px' }}>+</Button>
                <Button onClick={() => addNestedClause(index)} size='xs'><i className='fa fa-file'></i></Button>
                <Button onClick={() => resetOrRemoveClause(index)} size='xs' style={{ marginLeft: '5px' }}>-</Button>
            </> : <>
                <Button onClick={() => addSubClause(index)} size='xs' style={{ marginRight: '5px' }}>+</Button>
                <Button onClick={() => removeClause(index)} size='xs'>-</Button>
            </>
            }
        </InlineFieldRow>
    )
}

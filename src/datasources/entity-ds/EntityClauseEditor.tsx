import React, { useEffect } from 'react'
import { EntityClause } from './EntityClause'
import { ClauseActionType, OnmsEntityType, SearchOption } from './types'
import { API } from 'opennms'

interface EntityClauseEditorProps {
    setFilter: (filter: API.Filter) => void,
    loading: boolean
    propertiesAsArray: SearchOption[],
    clauses: any[],
    dispatchClauses: any
}

export const EntityClauseEditor = ({ setFilter, loading, propertiesAsArray, clauses, dispatchClauses }: EntityClauseEditorProps) => {

    useEffect(() => {
        const updatedFilter = new API.Filter();
        updatedFilter.limit = 10;

        // Build the filter
        clauses.forEach((d, i) => {
            // Handle comparator structure - could be { id, label } or { value: { i, l } }
            let comparatorValue = clauses[i].comparator?.value

            if (!comparatorValue && clauses[i].comparator?.id) {
                comparatorValue = {
                    id: clauses[i].comparator.id,
                    label: clauses[i].comparator.label,
                    l: clauses[i].comparator.label,
                    i: clauses[i].comparator.id
                }
            }

            if ((d.type === OnmsEntityType.AND || d.type === OnmsEntityType.FIRST) && comparatorValue) {
                updatedFilter.withAndRestriction(
                    new API.Restriction(
                        clauses[i].attribute?.id || clauses[i].attribute?.value?.id,
                        comparatorValue,
                        clauses[i].comparedString || clauses[i].comparedValue?.value
                    )
                )
            } else if (d.type === OnmsEntityType.OR && comparatorValue) {
                updatedFilter.withOrRestriction(
                    new API.Restriction(
                        clauses[i].attribute?.id || clauses[i].attribute?.value?.id,
                        comparatorValue,
                        clauses[i].comparedString || clauses[i].comparedValue?.value
                    )
                )
            }
        })
        
        setFilter(updatedFilter)
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [clauses])

    return (
        <>
            {clauses.map((clause, index) => (
                <React.Fragment key={index}>
                    <EntityClause
                        clause={clause}
                        setAttribute={(col, val) => dispatchClauses({ 
                            type: ClauseActionType.update, 
                            index: col, 
                            property: 'attribute', 
                            value: {
                                id: (val.value as any)?.id,
                                label: val.label,
                                value: val.value  // Preserve full metadata
                            }
                        })}
                        setComparator={(col, val) => dispatchClauses({ 
                            type: ClauseActionType.update, 
                            index: col, 
                            property: 'comparator', 
                            value: { 
                                id: (val.value as any)?.id || (val.value as any)?.i, 
                                label: val.label || (val.value as any)?.l 
                            } 
                        })}
                        setComparedValue={(col, val) => dispatchClauses({ 
                            type: ClauseActionType.update, 
                            index: col, 
                            property: 'comparedValue', 
                            value: { 
                                value: typeof val.value === 'object' && val.value !== null 
                                    ? (val.value as any).id || val.value 
                                    : val.value 
                            } 
                        })} 
                        setComparedString={(col, val) => dispatchClauses({ 
                            type: ClauseActionType.update, 
                            index: col, 
                            property: 'comparedString', 
                            value: val 
                        })}
                        setClauseType={(col, val) => dispatchClauses({ 
                            type: ClauseActionType.update, 
                            index: col, 
                            property: 'type', 
                            value: val 
                        })}
                        dispatchClauses={dispatchClauses}
                        loading={loading}
                        index={index}
                        propertiesAsArray={propertiesAsArray}
                        hasMultipleClauses={clauses.length > 1}
                    />
                    <div className='spacer' />
                </React.Fragment>
            ))}
        </>
    )
};

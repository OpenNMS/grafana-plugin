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

    const addNestedClause = (col: number) => {
        dispatchClauses({ type: ClauseActionType.addNestedClause, index: col });
    }

    const addClause = (col: number) => {
        dispatchClauses({ type: ClauseActionType.addClause, index: col });
    }

    const addSubClause = (col: number) => {
        dispatchClauses({ type: ClauseActionType.addSubClause, index: col });
    }

    const setClause = (col: number, value: any, property: string) => {
        dispatchClauses({ type: ClauseActionType.update, index: col, property, value });
    }

    const removeClause = (col: number) => {
        dispatchClauses({ type: ClauseActionType.delete, index: col });
    }

    useEffect(() => {
        const updatedFilter = new API.Filter();
        updatedFilter.limit = 10;
        //Build the filter. This could be extracted to a helper function.
        clauses.forEach((d, i) => {
            if (d.type === OnmsEntityType.AND || d.type === OnmsEntityType.FIRST && clauses[i].comparator?.value) {
                updatedFilter.withAndRestriction(
                    new API.Restriction(
                        clauses[i].attribute?.value?.id,
                        clauses[i].comparator?.value,
                        clauses[i].comparedString || clauses[i].comparedValue.value
                    )
                )
            } else if (d.type === OnmsEntityType.OR && clauses[i].comparator?.value) {
                updatedFilter.withOrRestriction(
                    new API.Restriction(
                        clauses[i].attribute?.value?.id,
                        clauses[i].comparator?.value,
                        clauses[i].comparedString || clauses[i].comparedValue.value
                    )
                )
            }
        })
        setFilter(updatedFilter);
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [clauses])

    return (
        <>
            {clauses.map((clause, index) => {
                return (
                    <>
                        <EntityClause
                            key={index}
                            clause={clauses[index]}
                            setAttribute={(col, val) => setClause(col, val, 'attribute')}
                            setComparator={(col, val) => setClause(col, val, 'comparator')}
                            setComparedValue={(col, val) => setClause(col, val, 'comparedValue')}
                            setComparedString={(col, val) => setClause(col, val, 'comparedString')}
                            setClauseType={(col, val) => setClause(col, val, 'type')}
                            removeClause={removeClause}
                            loading={loading}
                            index={index}
                            addClause={addClause}
                            addNestedClause={addNestedClause}
                            addSubClause={addSubClause}
                            propertiesAsArray={propertiesAsArray}
                            hasMultipleClauses={clauses.length > 1}
                        />

                        <div className='spacer' />
                    </>
                )
            })}
        </>)
};

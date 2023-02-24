import React, { useEffect } from 'react'
import { EntityClause } from './EntityClause'
import { OnmsEntityClause, OnmsEntityNestType, OnmsEntityType, SearchOption } from './types'
import { API } from 'opennms'
import { defaultClause } from './constants'

interface EntityClauseEditorProps {
    filter: API.Filter,
    setFilter: (filter: API.Filter) => void,
    loading: boolean
    propertiesAsArray: SearchOption[],
    clauses: any[],
    setClauses: any
}

export const EntityClauseEditor = ({ filter, setFilter, loading, propertiesAsArray, clauses, setClauses }: EntityClauseEditorProps) => {

    const addNestedClause = (col: number) => {
        const newClause = { ...defaultClause };
        newClause.nestingType = OnmsEntityNestType.NESTED
        newClause.type = OnmsEntityType.AND;
        actuallyAddClause(col, newClause);
    }

    const addClause = (col: number) => {
        const newClause = { ...defaultClause };
        newClause.nestingType = OnmsEntityNestType.TOP;
        newClause.type = OnmsEntityType.AND;
        actuallyAddClause(col, newClause);
    }

    const addSubClause = (col: number) => {
        const newClause = { ...defaultClause };
        newClause.nestingType = OnmsEntityNestType.SUB;
        newClause.type = OnmsEntityType.AND;
        actuallyAddClause(col, newClause);
    }

    const actuallyAddClause = (col: number, newClause: OnmsEntityClause) => {
        const newClauses = [...clauses];
        newClauses.splice(col + 1, 0, newClause);
        setClauses(newClauses);
    }

    const setClause = (col: number, value: any, property: string) => {
        const newAttributes = [...clauses]
        newAttributes[col][property] = value;
        setClauses(newAttributes);
    }

    const removeClause = (col: number) => {
        const newClauses = [...clauses];
        newClauses.splice(col, 1);
        if (newClauses[col] && newClauses[col].type !== clauses[col].type) {
            if (newClauses[col].nestingType !== clauses[col].nestingType) {
                newClauses[col].nestingType = clauses[col].nestingType
            }
            newClauses[col].type = clauses[col].type
        }
        setClauses(newClauses);

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

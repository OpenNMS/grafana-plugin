import { InlineFormLabel } from '@grafana/ui';
import React from 'react'
import { EntityClauseLabelProps, OnmsEntityNestType, OnmsEntityType } from './types';
const FormWithStyle = InlineFormLabel as any;

export const EntityClauseLabel = ({ type, nestingType, index, setClauseType }: EntityClauseLabelProps) => {

    const boxLabel = () => {
        let topLabel = '';
        if (nestingType === OnmsEntityNestType.TOP && type === OnmsEntityType.FIRST) {
            topLabel = 'WHERE'
        } else if ((nestingType === OnmsEntityNestType.TOP || nestingType === OnmsEntityNestType.NESTED) && type === OnmsEntityType.AND) {
            topLabel = 'AND'
        } else if ((nestingType === OnmsEntityNestType.TOP || nestingType === OnmsEntityNestType.NESTED) && type === OnmsEntityType.OR) {
            topLabel = 'OR'
        } else if (nestingType === OnmsEntityNestType.SUB) {
            topLabel = ''
        }
        return topLabel;
    }

    const boxSubLabel = () => {
        let label = ''
        if (nestingType === OnmsEntityNestType.NESTED) {
            label = 'WHERE'
        }
        return label;
    }

    const boxSubSubLabel = () => {
        let label = ''
        if (type === OnmsEntityType.AND) {
            label = 'AND'
        } else if (type === OnmsEntityType.OR) {
            label = 'OR'
        }
        return label;
    }

    const label = boxLabel();
    const subLabel = boxSubLabel();
    const subsubLabel = boxSubSubLabel();

    return (
        <>
            {
                label && 
                <FormWithStyle style={{ color: 'rgb(110, 159, 255)', maxWidth: '96px', cursor: label === 'WHERE' ? 'auto' : 'pointer' }}
                    onClick={() => {
                        if (label === 'AND') {
                            setClauseType(index, 1)
                        } else if (label === 'OR') {
                            setClauseType(index, 0)
                        }
                    }}>
                    {label}
                </FormWithStyle>
            }
            {
                nestingType === OnmsEntityNestType.NESTED &&
                <FormWithStyle style={{ color: 'rgb(110, 159, 255)', maxWidth: '96px', cursor: subLabel === 'WHERE' ? 'auto' : 'pointer' }}
                    onClick={() => {
                        if (label === 'AND') {
                            setClauseType(index, 1)
                        } else if (label === 'OR') {
                            setClauseType(index, 0)
                        }
                    }}>
                    {subLabel}
                </FormWithStyle>
            }
            {
                nestingType === OnmsEntityNestType.SUB &&
                <>
                    <div style={{ minWidth: '100px' }}>&nbsp;</div>
                    <FormWithStyle style={{ color: 'rgb(110, 159, 255)', maxWidth: '96px', cursor: subLabel === 'WHERE' ? 'auto' : 'pointer' }}
                        onClick={() => {
                            if (subsubLabel === 'AND') {
                                setClauseType(index, 1)
                            } else if (subsubLabel === 'OR') {
                                setClauseType(index, 0)
                            }
                        }}>
                        {subsubLabel}
                    </FormWithStyle>
                </>
            }
        </>
    )
}

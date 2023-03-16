import React, { useEffect, useState } from 'react'
import {
    Button,
    Segment,
    SegmentSection,
    Tag,
} from '@grafana/ui'
import { SelectableValue } from '@grafana/data'

import { API } from 'opennms'
import { defaultOrder } from './constants'
import { SearchOption } from './types'

interface EntityOrderByEditorProps {
    filter: API.Filter,
    /** callback to update filter when order by values have changed */
    setFilter: (f: API.Filter) => void,
    /** list of search attributes to order by */
    searchAttributes: SearchOption[],
}

export const EntityOrderByEditor = ({ filter, setFilter, searchAttributes }: EntityOrderByEditorProps) => {

    const [directionValue, setDirectionValue] = useState<SelectableValue<string>>(defaultOrder)
    const [selectionValue,] = useState<SelectableValue<string>>({ label: '' })
    const [tags, setTags] = useState([] as Array<SelectableValue<string>>)
    const [isAdding, setIsAdding] = useState(false)
    const [selectionProperties, setSelectionProperties] = useState([] as Array<SelectableValue<string>>)

    useEffect(() => {
        updateFilter()
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [tags, directionValue])

    /** User clicked on order by attribute, remove it. */
    const onTagClick = (name) => {
        const newTags = tags.filter((tag) => tag.label && tag.label !== name)
        setTags(newTags)
    }

    const addTag = (item: SelectableValue<string>) => {
        if (item.value) {
            const newTags = tags.concat(item)
            setTags(newTags)
            setIsAdding(false)
        }
    }

    const updateSelectionProperties = () => {
        const tagIds = tags.map(t => t.value || '')

        const newProperties = searchAttributes
            .filter(p => !tagIds.includes(p.label))
            .map(p => ({ label: p.name, value: p.id }))

        setSelectionProperties(newProperties)
    }

    const enterAddMode = () => {
        updateSelectionProperties()
        setIsAdding(true)
    }

    const updateFilter = () => {
        const order = directionValue.label === 'ASC' ? API.Orders.ASC : API.Orders.DESC
        const newOrderBy = tags.map(item => new API.OrderBy(item.value || '', order))

        setFilter({
            ...filter,
            orderBy: newOrderBy
        })
    }

    return (
        <SegmentSection label='ORDER BY'>
            <Segment
                allowEmptyValue={false}
                value={directionValue}
                onChange={(item) => {
                    setDirectionValue(item)
                }}
                options={[{ label: 'DESC' }, { label: 'ASC' }]}
            />
            {
                tags &&
                tags.map((item, i) => (
                    <Tag
                        name={item.label || ''}
                        key={i}
                        icon='x'
                        style={{ marginLeft: '4px', marginRight: '2px' }}
                        onClick={onTagClick}
                    />
                ))
            }
            {
                isAdding &&
                <Segment
                    allowEmptyValue={false}
                    onBlur={() => setIsAdding(false)}
                    onChange={(item) => addTag(item)}
                    options={selectionProperties}
                    placeholder='Select attribute...'
                    value={selectionValue}
                />
            }
            {
                !isAdding &&
                <Button onClick={() => enterAddMode()} size='xs'>+</Button>
            }
        </SegmentSection>
    )
}

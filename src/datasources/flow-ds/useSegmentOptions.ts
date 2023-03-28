import { SelectableValue } from '@grafana/data'
import { useState, useEffect } from 'react'
import { defaultSegmentOptions, FlowFunctions } from './constants'
import { filterOutExcludedFunctions, filterOutExistingSingleUseFunctions, filterOutItemsMissingActiveMetric } from './helpers'
import { FlowFunction } from './types'

export const useSegmentOptions = (selectedOptions: Array<SelectableValue<string>> | undefined, segmentValue: SelectableValue<number> | undefined) => {
    const [options, setOptions] = useState(defaultSegmentOptions);
    const [excludedFunctions, setExcludedFunctions] = useState<number[]>([]);
    const [parentSegments, setParentSegments] = useState<number[]>([]);

    useEffect(() => {
        if (selectedOptions) {
            setExcludedFunctions([])
            setParentSegments([])
            for (let activeFunction of selectedOptions) {
                if (activeFunction.label) {
                    const { excludeFunctions, parentSegments } = FlowFunctions.get(activeFunction.label) || {} as FlowFunction;
                    if (excludeFunctions) {
                        setExcludedFunctions((excluded) => ([...new Set(excluded.concat(excludeFunctions))])) // ...new Set for unique
                    }
                    if (parentSegments) {
                        setParentSegments((parent) => ([...new Set(parent.concat(parentSegments))])) // ..new Set for unique
                    }
                }
            }
        }
    }, [selectedOptions])

    useEffect(() => {

        //Filter out excluded functions
        let newOptions = filterOutExcludedFunctions(excludedFunctions);


        //IF we have a metric selected, filter out items that have a parentsegment set, but don't match the selected metric.
        if (segmentValue) {
            newOptions = filterOutItemsMissingActiveMetric(newOptions,segmentValue);
           
        }

        //Filter out items not marked for multiple use if they are already in use
        if (selectedOptions && selectedOptions?.length > 0) {
            newOptions = filterOutExistingSingleUseFunctions(newOptions,selectedOptions);
        }

        
        newOptions = newOptions.filter((topOption) => topOption.options.length > 0) //Filter out top level items that don't have any results

        setOptions(newOptions);
    }, [excludedFunctions, parentSegments, segmentValue, selectedOptions])

    return { options }
}

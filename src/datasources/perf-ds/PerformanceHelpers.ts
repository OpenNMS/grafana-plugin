import { ArrayVector, DataFrame, Field, FieldType } from "@grafana/data"
import { FunctionFormatter } from "lib/function_formatter";
import { OnmsMeasurementsQueryResponse } from "./types";

/**
 * Get 'windowed' timestamps that are between start/end range.
 * @param timestamps an array of timestamp values
 * @param start a starting timestamp for the window, or 0 to start at the beginning
 * @param end an ending timestamp for the window, or 0 to not limit the end of the window range
 */
const getWindowedTimestamps = (timestamps: number[], start: number, end: number) => {
    if (start === 0 && end === 0) {
        return {
            windowedTimestamps: [...timestamps],
            startIndex: 0,
            endIndex: timestamps.length - 1
        }
    }

    const windowedTimestamps = [] as number[]
    let startIndex = -1
    let endIndex = timestamps.length - 1

    for (let i = 0; i < timestamps.length; i++) {
        const tsVal = timestamps[i]

        if (end > 0 && tsVal > end) {
            endIndex = i - 1
            break
        }

        if (start === 0 || tsVal >= start) {
            if (startIndex < 0) {
                startIndex = i
            }
            windowedTimestamps.push(tsVal)
        }
    }

    return {
        windowedTimestamps,
        startIndex,
        endIndex
    }
}

/**
 * Convert QueryResponse data returned by OpenNMS Measurements Rest API to Grafana DataFrame format.
 */
export const measurementResponseToDataFrame =
    (measurementData: OnmsMeasurementsQueryResponse): DataFrame[] => {

        const { start, end, labels, columns, timestamps, metadata } = measurementData

        const dataFrames: DataFrame[] = []

        let dataFrame: DataFrame = getEmptyDataFrame()

        if (!timestamps || !timestamps.length) {            
            dataFrames.push(dataFrame)
        } else {
            const { windowedTimestamps, startIndex, endIndex } = getWindowedTimestamps(timestamps, start, end)

            // no data or no data within the start/end timespan, return an empty DataFrame
            if (windowedTimestamps.length === 0) {
                dataFrames.push(dataFrame)
            } else {

                for (let i = 0; i < labels?.length; i++) {
                    dataFrame = getEmptyDataFrame()

                    dataFrame.length = windowedTimestamps.length

                    dataFrame.fields.push({
                        name: 'Time',
                        type: FieldType.time,
                        config: {},
                        values: new ArrayVector<number>(windowedTimestamps)
                    } as Field)

                    const label = metadata && metadata.resources ?
                        FunctionFormatter.format(labels[i], metadata) :
                        labels[i]

                    if (columns && columns.length) {

                        const column = columns[i]
                        const windowedValues = column.values.slice(startIndex, endIndex + 1)

                        let field = {
                            name: label || 'Value',
                            type: FieldType.number, // number but actual data may be a string representing a number or "NaN"
                            config: {},
                            values: new ArrayVector<string | number | null>(windowedValues)
                        } as Field
                        dataFrame.fields.push(field)
                    }
                    dataFrames.push(dataFrame)
                }
            }
        }

        return dataFrames
    }

const getEmptyDataFrame = () => {
    return {
        name: '',
        length: 0,
        fields: []
    }
}

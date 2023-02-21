import { ArrayVector, DataFrame, Field, FieldType } from "@grafana/data"
import { FunctionFormatter } from "lib/function_formatter";
import { OnmsMeasurementsQueryResponse } from "./types";

/**
 * Convert QueryResponse data returned by OpenNMS Measurements Rest API to Grafana DataFrame format.
 */
export const measurementResponseToDataFrame =
    (measurementData: OnmsMeasurementsQueryResponse, queryRefId?: string): DataFrame => {

    const { start, end, labels, columns, timestamps, metadata } = measurementData

    const dataFrame: DataFrame = {
        name: '',
        refId: queryRefId,
        length: 0,
        fields: []
    }

    if (!columns || !columns.length || !timestamps || !timestamps.length) {
        return dataFrame
    }

    // timestamps
    let windowedTimestamps = [] as number[]
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

    // no data or no data within the start/end timespan, return an empty DataFrame
    if (windowedTimestamps.length === 0) {
        return dataFrame
    }

    dataFrame.length = windowedTimestamps.length

    dataFrame.fields.push({
        name: 'Time',
        type: FieldType.time,
        config: {},
        values: new ArrayVector<number>(windowedTimestamps)
    } as Field)

    for (let i = 0; i < columns?.length; i++) {
        let label = metadata && metadata.resources ?
            FunctionFormatter.format(labels[i], metadata) :
            labels[i];

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

    return dataFrame
}
